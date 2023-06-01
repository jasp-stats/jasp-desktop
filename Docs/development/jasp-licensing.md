
JASP Licensing
==============

JASP is made up of two parts, or executables. The *JASP* graphical user interface, and the *JASP engine* responsible for running the analyses in the background.

 - As an executable binary, the JASP graphical user interface is released under the [GNU Affero General Public License, version 3](https://www.gnu.org/licenses/agpl.txt)
 - As an executable binary, the JASP engine is released under the [GNU General Public License, version 2](https://www.gnu.org/licenses/gpl2.txt)

The JASP source code itself is licensed:

 - Common: GPL2+
 - Engine: GPL2+
 - Desktop: AGPL3+

The JASP graphical user interface, with the filename *JASP*, is built from the *Desktop* and *Common* folders in the JASP source tree. As a binary, it is released under the AGPL3 (Desktop and Common can be combined because GPL2+ can be treated as GPL3, and the GPL3 and the AGPL3 are compatible)

The JASP engine, with the filename *JASPEngine* is built from the *Engine* and *Common* folders in the JASP source tree. As a binary, it is released under the GPL2 (and cannot be released under the GPL3 because, at present, it has a number of R package dependencies which are GPL2 only).

Contributing to JASP
====================

Individuals contributing to the JASP project should ensure that correct copyright and license statements are added to the beginnings of all source files.

Copyright declarations in source code
-------------------------------------

JASP source files should contain a copyright declaration (followed by the license details). If you are contributing as a *University of Amsterdam* employee, the University holds the copyright, and the copyright assignment should be something like:

    // Copyright (C) 2013-2015 University of Amsterdam

If you are contributing a new file (that you have written), and are not an employee of the University of Amsterdam, then you should place a copyright assignment with your name:

    // Copyright (C) 2015 John Smith

If you are modifying or adding to an existing file in the JASP project, then simply add your name to the copyright holders:

    // Copyright (C) 2013-2015 University of Amsterdam, John Smith


License statement in source code
--------------------------------

Following the copyright statement, a statement of the license should follow.

### Desktop

.c, .cpp, and .h files in Desktop should have the following AGPL3+ header (year and copyright holders may vary).

    //
    // Copyright (C) 2015 University of Amsterdam
    //
    // This program is free software: you can redistribute it and/or modify
    // it under the terms of the GNU Affero General Public License as
    // published by the Free Software Foundation, either version 3 of the
    // License, or (at your option) any later version.
    //
    // This program is distributed in the hope that it will be useful,
    // but WITHOUT ANY WARRANTY; without even the implied warranty of
    // MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    // GNU Affero General Public License for more details.
    //
    // You should have received a copy of the GNU Affero General Public
    // License along with this program.  If not, see
    // <http://www.gnu.org/licenses/>.
    //

### Engine, Common

.c, .cpp, and .h files in Engine and Common should have the following GPL2+ header (year and copyright holders may vary).

    //
    // Copyright (C) 2015 University of Amsterdam
    //
    // This program is free software: you can redistribute it and/or modify
    // it under the terms of the GNU General Public License as published by
    // the Free Software Foundation, either version 2 of the License, or
    // (at your option) any later version.
    //
    // This program is distributed in the hope that it will be useful,
    // but WITHOUT ANY WARRANTY; without even the implied warranty of
    // MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    // GNU General Public License for more details.
    //
    // You should have received a copy of the GNU General Public License
    // along with this program.  If not, see <http://www.gnu.org/licenses/>.
    //

### Engine/JASP/R

.R files in Engine/JASP/R/* should have the following GPL2+ header (note that this is the same as above, but the lines begin with #'es) (year and copyright holders may vary).

    #
    # Copyright (C) 2013-2015 University of Amsterdam
    #
    # This program is free software: you can redistribute it and/or modify
    # it under the terms of the GNU General Public License as published by
    # the Free Software Foundation, either version 2 of the License, or
    # (at your option) any later version.
    #
    # This program is distributed in the hope that it will be useful,
    # but WITHOUT ANY WARRANTY; without even the implied warranty of
    # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    # GNU General Public License for more details.
    #
    # You should have received a copy of the GNU General Public License
    # along with this program.  If not, see <http://www.gnu.org/licenses/>.
    #



