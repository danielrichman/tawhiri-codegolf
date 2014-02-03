# Copyright (C) 2014 ??
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
Forwards Euler Integration
"""


from __future__ import unicode_literals, print_function, division

import numpy as np

from . import Solver, wrap_longitude
from .. import Time


class ForwardsEuler(Solver):
    """Forwards Euler ODE solver, with time step `dt`"""

    def __init__(self, dt):
        self.dt = dt

    def __call__(self, initial_conditions, model, termination_function):
        x = np.array(initial_conditions.x, dtype='float')
        t = Time.from_initial_conditions(initial_conditions, 0)

        while True:
            if termination_function(x, t):
                break

            x_dot = np.array(model(x, t))
            x += x_dot * self.dt
            x[1] = wrap_longitude(x[1])
            t += self.dt

            yield x, t

class ForwardsEulerWithAP(Solver):
    """Forwards Euler ODE solver, with an altitude profile & time step `dt`"""

    def __init__(self, dt):
        self.dt = dt

    def __call__(self, initial_conditions, model, altitude_profile,
                       termination_function):
        x = np.array(initial_conditions.x, dtype='float')
        t = Time.from_initial_conditions(initial_conditions, 0)

        while True:
            if termination_function(x, t):
                break

            x_dot = np.array(model(x, t))
            assert x_dot[2] == 0

            x += x_dot * self.dt
            x[1] = wrap_longitude(x[1])
            x[2] = altitude_profile(t)
            t += self.dt

            yield x, t
