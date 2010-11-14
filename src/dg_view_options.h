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

#ifndef DG_VIEW_OPTIONS_H
#define DG_VIEW_OPTIONS_H

#include <set>
#include <string>
#include <stdint.h>

#define CONDITION_SET_FLAG_ANY           1U
#define CONDITION_SET_FLAG_MALLOC        2U /* Any malloc-like function */
#define CONDITION_SET_FLAG_RANGE         4U /* Any TRACK_RANGE request */

class dg_view_options
{
public:
    /* Set of conditions to match stack traces against based on command-line options */
    struct condition_set
    {
        uint32_t flags;         /* set-specific flags, not generically used */
        std::set<std::string> user;
        std::set<std::string> functions;
        std::set<std::string> files;
        std::set<std::string> dsos;
    };

private:
    /* Events selected on the command line */
    condition_set event_conditions;
    /* Ranges selected on the command line */
    condition_set block_conditions;

public:
    void parse_opts(int *argc, char **argv);

    const condition_set &get_event_conditions() const;
    const condition_set &get_block_conditions() const;
};

void dg_view_usage(const char *prog, int code);

#endif /* DG_VIEW_OPTIONS_H */
