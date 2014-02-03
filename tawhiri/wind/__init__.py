# Copyright (C) 2013 Daniel Richman
#
# This file is part of tawhiri.
#
# tawhiri is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# tawhiri is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with tawhiri.  If not, see <http://www.gnu.org/licenses/>.

"""
Wind Datasets

Datasets downloaded from the NOAA are stored as large binary files that are
memmapped into the predictor process and thereby treated like a huge array.
"""


from __future__ import unicode_literals, print_function, division

import logging
from collections import namedtuple
import os
import os.path
import itertools
import warnings
from datetime import datetime
import numpy as np


__all__ = ["Dataset", "unpack_grib"]


logger = logging.getLogger("tawhiri.wind")


class OutOfRangeError(LookupError):
    """A variable was out of range (when interpolating)."""
    def __init__(self, variable, value):
        super(OutOfRangeError, self).__init__(variable, value)
        self.variable = variable
        self.value = value

class OutOfRangeWarning(Warning):
    """A variable was out of range, and so had to be extrapolated."""
    def __init__(self, altitude, lerp):
        super(OutOfRangeWarning, self).__init__()
        self.altitude = altitude
        self.lerp = lerp


class Dataset(object):
    """
    A thin wrapper around :func:`np.memmap` that opens wind Datasets.

    .. attribute:: shape

        The shape of a dataset; i.e., the number of points on each axes.

        Note ``len(axes[i]) == shape[i]``.

    .. attribute:: axes

        The axes of the dataset, a (named) tuple containing the 5 axes
        (`hour`, `pressure`, `variable`, `latitude` and `longitude`)
        and the values of the points along them.

        For example, ``axes.pressure[4]`` is ``900`` - so points in
        cells ``dataset.array[a][4][b][c][d]`` correspond to data at 900mb.

    .. attribute:: SUFFIX_GRIBMIRROR

        The suffix to use when opening a "GRIB mirror" file; see
        the `suffix` argument to :method:`filename`.

    """

    shape = (65, 47, 3, 361, 720)

    # TODO: use the other levels too?
    # {10, 80, 100}m heightAboveGround (u, v)
    #       -- note ground, not mean sea level - would need elevation
    # 0 unknown "planetary boundry layer" (u, v) (first two records)
    # 0 surface "Planetary boundary layer height"
    # {1829, 2743, 3658} heightAboveSea (u, v)
    pressures_pgrb2f = [10, 20, 30, 50, 70, 100, 150, 200, 250, 300, 350, 400,
                        450, 500, 550, 600, 650, 700, 750, 800, 850, 900, 925,
                        950, 975, 1000]
    pressures_pgrb2bf = [1, 2, 3, 5, 7, 125, 175, 225, 275, 325, 375, 425,
                         475, 525, 575, 625, 675, 725, 775, 825, 875]

    _axes_type = namedtuple("axes",
                ("hour", "pressure", "variable", "latitude", "longitude"))

    axes = _axes_type(
        range(0, 192 + 3, 3),
        sorted(pressures_pgrb2f + pressures_pgrb2bf, reverse=True),
        ["height", "wind_u", "wind_v"],
        np.arange(-90, 90 + 0.5, 0.5),
        np.arange(0, 360, 0.5)
    )

    _listdir_type = namedtuple("dataset_in_row",
                ("ds_time", "suffix", "filename", "path"))

    assert shape == tuple(len(x) for x in axes)

    SUFFIX_GRIBMIRROR = '.gribmirror'

    @classmethod
    def filename(cls, directory, ds_time, suffix=''):
        """The path to a dataset file in `directory` with datetime `ds_time`"""
        ds_time_str = ds_time.strftime("%Y%m%d%H")
        return os.path.join(directory, ds_time_str + suffix)

    @classmethod
    def listdir(cls, directory, only_suffices=None):
        """
        Scan for datasets in `directory`

        ... with filenames matching those generated by :classmethod:`filename`

        ... and a suffix in `only_suffices` (or any suffix, if unspecified)

        Returns tuples ``(dataset time, suffix, filename, full path)``
        (named tuple; attributes also accessible as `ds_time`, `suffix`,
        `filename` and `path`).
        """

        for filename in os.listdir(directory):
            if len(filename) < 10:
                continue

            ds_time_str = filename[:10]
            try:
                ds_time = datetime.strptime(ds_time_str, "%Y%m%d%H")
            except ValueError:
                pass
            else:
                suffix = filename[10:]
                if only_suffices and suffix not in only_suffices:
                    continue

                yield cls._listdir_type(ds_time, suffix, filename,
                                        os.path.join(directory, filename))

    @classmethod
    def checklist(cls):
        """
        Create a matrix of bools with dimensions ``shape[0:3]``

        ... i.e., a element for every GRIB record we need when downloading
        a new dataset
        """
        return np.zeros(cls.shape[0:3], dtype=np.bool_)

    def __init__(self, directory, ds_time, suffix='', new=False):
        """
        Open the dataset file for `ds_time`, in `directory`

        Creates a new file if `new` is set, overwriting an existing file
        if it existed.
        """

        self.directory = directory
        self.ds_time = ds_time
        self.new = new

        self.fn = self.filename(self.directory, self.ds_time, suffix)

        logger.info("Opening dataset %s %s %s", self.ds_time, self.fn,
                        '(truncate and write)' if new else '(read)')

        # TODO: is there enough precision in these values to warrant use of
        # float64? Would float32 not suffice?
        self.array = np.memmap(self.fn, mode=('w+' if self.new else 'r'),
                               dtype=np.float64, shape=self.shape, order='C')

    def interpolate(self, hour, latitude, longitude, altitude):
        """
        Return a (u, v) tuple for the specified point by interpolating.
        
        The units of `hour`, `latitude`, `longitude` are the same as that of
        the dataset, which crucially means that time is specified as a
        fraction of a hour into the dataset.

        `altitude` is in metres (units of the dataset "height" measurement),
        two pressure levels will be chosen around it.
        """

        def _pick(name, value, left, step, n):
            a = (value - left) / step
            b = int(a)
            if not 0 <= b < n - 1:
                raise OutOfRangeError(name, value)
            l = a - b
            return (b, 1 - l), (b + 1, l)

        ipl_hour = _pick("hour", hour, 0, 3, self.shape[0])
        ipl_lat  = _pick("latitude", latitude, -90, 0.5, self.shape[3])
        ipl_lon  = _pick("longitude", longitude, 0, 0.5, self.shape[4] + 1)
        if ipl_lon[0][0] == self.shape[4] - 1:
            left, (idx, lerp) = ipl_lon
            ipl_lon = left, (0, lerp)

        assert self.axes.hour[ipl_hour[0][0]] <= hour <= self.axes.hour[ipl_hour[1][0]]
        assert self.axes.latitude[ipl_lat[0][0]] <= latitude <= self.axes.latitude[ipl_lat[1][0]]
        if ipl_lon[1][0] == 0:
            r = 360
        else:
            r = self.axes.longitude[ipl_lon[1][0]]
        assert self.axes.longitude[ipl_lon[0][0]] <= longitude <= r

        levels = np.zeros(self.shape[1:3])
        for ipl in itertools.product(ipl_hour, ipl_lat, ipl_lon):
            (hour_idx, lat_idx, lon_idx), lerps = zip(*ipl)
            lerp = np.prod(lerps)
            levels += lerp * self.array[hour_idx,:,:,lat_idx,lon_idx]


        alt_idx = np.searchsorted(levels[:,0], altitude) - 1
        alt_lerp_warning = False

        if alt_idx == -1:
            alt_idx += 1
        if alt_idx == self.shape[1] - 1:
            alt_idx -= 1

        lower, upper = levels[(alt_idx, alt_idx + 1), :]
        alt_lower, alt_upper = lower[0], upper[0]

        step = upper[0] - lower[0]
        assert step > 0
        lerp = (altitude - lower[0]) / step

        # allow the lerp to go out of range a bit - we don't have
        # low-altitude data
        if not -1 <= lerp <= 1:
            raise OutOfRangeError("altitude", altitude)
        if not 0 <= lerp <= 1:
            warnings.warn(OutOfRangeWarning(altitude, lerp))

        return tuple((1 - lerp) * lower[1:] + lerp * upper[1:])

    def __del__(self):
        self.close()

    def close(self):
        """Close the dataset"""

        if hasattr(self, 'array'):
            logger.info("Closing dataset %s %s", self.ds_time, self.fn)
            del self.array
