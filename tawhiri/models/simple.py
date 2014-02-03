# Copyright (C) 2014 Daniel Richman
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

"""A collection of basic Models"""


from __future__ import unicode_literals, print_function, division

import numpy as np

from . import HorizontalModel


class Wind(HorizontalModel):
    """A horizontal model that assumes the balloon moves at wind speed"""

    _PI_180 = np.pi / 180
    _180_PI = 180 / np.pi

    def __init__(self, dataset):
        self.dataset = dataset

    def __call__(self, x, t):
        lat, lon, alt = x

        offset = (t.now - self.dataset.ds_time).total_seconds()
        wind_u, wind_v = self.dataset.interpolate(offset / 3600, lat, lon, alt)

        R = 6371009 + alt
        # arctan is approximately linear...
        dlat = self._180_PI * wind_v / R
        dlon = self._180_PI * wind_u / (R * np.cos(lat * self._PI_180))
        return dlat, dlon, 0
