/*
   This file is part of Datagrind, a tool for tracking data accesses.

   Copyright (C) 2010 Bruce Merry
      bmerry@users.sourceforge.net

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

#ifndef DG_VIEW_DEBUGINFO_H
#define DG_VIEW_DEBUGINFO_H

#include "dg_view_base.h"
#include "dg_view.h"
#include <string>

/* Load symbols from an object file, overriding the actual VMA of .text. */
void dg_view_load_object_file(const std::string &filename, address_type text_avma);

/* Turn a code address into a machine-readable location.
 * Unknown fields are set to empty/zero.
 */
void dg_view_addr2info(address_type addr, std::string &function, std::string &file, int &line, std::string &dso);

/* Turn a code address into a human-friendly form */
std::string dg_view_addr2line(address_type addr);

/* Takes a full path and returns a short form of it for display by stripping
 * paths.
 */
std::string dg_view_abbrev_file(const std::string &full);

/* Like dg_view_abbrev_file, but takes the name of a DSO. Currently
 * equivalent.
 */
std::string dg_view_abbrev_dso(const std::string &full);

#endif /* DG_VIEW_DEBUGINFO_H */
