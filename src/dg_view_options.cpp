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

#include <vector>
#include <string>
#include <cstdlib>
#include <cassert>
#include <stdio.h>
#include <getopt.h>
#include "dg_view_options.h"

using namespace std;

void dg_view_usage(const char *prog, int code)
{
    fprintf(stderr, "Usage: %s [-m] [--ranges=r1,...] [--events=e1,...] <file>\n", prog);
    exit(code);
}

/* Splits a string to pieces on commas. Empty parts are preserved, but if
 * s is empty then no strings are returned.
 */
static vector<string> split_comma(const string &s)
{
    vector<string> out;
    string::size_type pos = 0;

    if (s.empty()) return out;
    while (true)
    {
        string::size_type next = s.find(',', pos);
        if (next == string::npos)
        {
            /* End of string - capture the last element */
            out.push_back(s.substr(pos));
            return out;
        }
        out.push_back(s.substr(pos, next - pos));
        pos = next + 1;
    }
}

static void condition_set_parse(dg_view_options::condition_set &cs, const vector<string> &tokens, uint32_t valid_flags)
{
    for (vector<string>::const_iterator i = tokens.begin(); i != tokens.end(); ++i)
    {
        const string &s = *i;
        if ((valid_flags & CONDITION_SET_FLAG_MALLOC) && s == "malloc")
            cs.flags |= CONDITION_SET_FLAG_MALLOC;
        else if ((valid_flags & CONDITION_SET_FLAG_RANGE) && s == "range")
            cs.flags |= CONDITION_SET_FLAG_RANGE;
        else if (s.substr(0, 3) == "fn:")
            cs.functions.insert(s.substr(3));
        else if (s.substr(0, 5) == "file:")
            cs.files.insert(s.substr(5));
        else if (s.substr(0, 4) == "dso:")
            cs.dsos.insert(s.substr(4));
        else if (s.substr(0, 5) == "user:")
            cs.user.insert(s.substr(5));
        else
        {
            fprintf(stderr, "Invalid condition `%s'\n", s.c_str());
            exit(2);
        }
    }
}

void dg_view_options::parse_opts(int *argc, char **argv)
{
    static const struct option longopts[] =
    {
        { "ranges", 1, NULL, 'r' },
        { "events", 1, NULL, 'e' },
        { NULL, 0, NULL, 0 }
    };
    int opt;

    event_conditions.flags = CONDITION_SET_FLAG_ANY;
    block_conditions.flags = CONDITION_SET_FLAG_ANY;
    while ((opt = getopt_long(*argc, argv, "r:e:", longopts, NULL)) != -1)
    {
        switch (opt)
        {
        case 'r':
            {
                vector<string> ranges = split_comma(optarg);
                condition_set_parse(block_conditions, ranges, CONDITION_SET_FLAG_MALLOC | CONDITION_SET_FLAG_RANGE);
                block_conditions.flags &= ~CONDITION_SET_FLAG_ANY;
            }
            break;
        case 'e':
            {
                vector<string> events = split_comma(optarg);
                condition_set_parse(event_conditions, events, 0);
                event_conditions.flags &= ~CONDITION_SET_FLAG_ANY;
            }
            break;
        case '?':
        case ':':
            exit(2);
        default:
            assert(0);
        }
    }

    /* Remove options from argv */
    for (int i = optind; i < *argc; i++)
        argv[i - optind + 1] = argv[i];
    *argc -= optind - 1;
}

const dg_view_options::condition_set &dg_view_options::get_event_conditions() const
{
    return event_conditions;
}

const dg_view_options::condition_set &dg_view_options::get_block_conditions() const
{
    return block_conditions;
}
