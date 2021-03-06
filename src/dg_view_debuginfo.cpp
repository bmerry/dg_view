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

#include "dg_view.h"
#include "dg_view_debuginfo.h"
#include <cstdlib>
#include <cstring>
#include <map>
#include <string>
#include <sstream>
#include <vector>
#include <memory>
#include <bfd.h>

using namespace std;

namespace
{

/* Corresponds to either the main file or the .gnu_debuglink file */
class object_subfile
{
public:
    char *filename;  /* bfd uses this storage rather than making its own */
    bfd *abfd;
    vector<asymbol *> syms;

private:
    /* Prevent copying */
    object_subfile(const object_subfile &);
    object_subfile &operator=(const object_subfile &);

public:
    object_subfile() : filename(NULL), abfd(NULL), syms() {}
    ~object_subfile()
    {
        delete[] filename;
        if (abfd != NULL)
            bfd_close(abfd);
    }

    bool load(const std::string &filename);
};

class object_file
{
public:
    address_type text_avma;
    object_subfile *subfiles[2];

    object_file() : text_avma(0)
    {
        subfiles[0] = subfiles[1] = NULL;
    }

    ~object_file()
    {
        delete subfiles[0];
        delete subfiles[1];
    }

private:
    /* Prevent copying */
    object_file(const object_file &);
    object_file &operator=(const object_file &);
};

/* TODO: this never gets cleaned up */
static map<string, object_file *> object_files;

/* Loads either the main file or the .gnu_debuglink file */
bool object_subfile::load(const std::string &filename)
{
    char **matching;
    long symcount = 0, storage;
    size_t filename_len = filename.size();

    delete[] this->filename;
    if (abfd != NULL)
        bfd_close(abfd);

    this->filename = new char[filename_len + 1];
    strcpy(this->filename, filename.c_str());
    abfd = bfd_openr(this->filename, NULL);
    if (!abfd)
        return false;
    if (!bfd_check_format_matches(abfd, bfd_object, &matching))
        return false;

    storage = bfd_get_symtab_upper_bound(abfd);
    if (storage > 0)
    {
        syms.resize(storage);
        symcount = bfd_canonicalize_symtab(abfd, &syms[0]);
    }

    if (symcount == 0)
    {
        storage = bfd_get_dynamic_symtab_upper_bound(abfd);
        if (storage > 0)
        {
            syms.resize(storage);
            symcount = bfd_canonicalize_dynamic_symtab(abfd, &syms[0]);
        }
    }
    if (symcount <= 0)
    {
        bfd_close(abfd);
        abfd = NULL;
        return false;
    }

    return true;
}

} /* namespace */

void dg_view_load_object_file(const std::string &filename, address_type text_avma)
{
    auto_ptr<object_subfile> primary(new object_subfile);
    if (!primary->load(filename))
        return;

    object_file *&of = object_files[filename];
    if (of == NULL)
        of = new object_file;
    of->text_avma = text_avma;
    delete of->subfiles[0];
    delete of->subfiles[1];
    of->subfiles[0] = primary.release();
    of->subfiles[1] = NULL;

    char *gnu_debuglink = bfd_follow_gnu_debuglink(of->subfiles[0]->abfd, "/usr/lib/debug");
    if (gnu_debuglink != NULL)
    {
        auto_ptr<object_subfile> secondary(new object_subfile);
        if (secondary->load(gnu_debuglink))
            of->subfiles[1] = secondary.release();
        free(gnu_debuglink);
    }
}

struct addr2info_info
{
    address_type addr;
    object_file *obj;
    object_subfile *sub;
    bool found;
    const char *source;
    const char *function;
    unsigned int line;
};

static void addr2info_section(bfd *abfd, asection *sect, void *arg)
{
    addr2info_info *info = (addr2info_info *) arg;

    if (info->found)
        return;
    if ((bfd_get_section_flags(abfd, sect) & SEC_ALLOC) == 0)
        return;

    bfd_size_type size = bfd_get_section_size(sect);
    if (info->addr >= info->obj->text_avma + size)
        return;

    if (!bfd_find_nearest_line(abfd, sect, &info->sub->syms[0],
                               info->addr - info->obj->text_avma,
                               &info->source, &info->function,
                               &info->line))
    {
        info->source = NULL;
        info->function = NULL;
        info->line = 0;
    }
    info->found = true;
}

void dg_view_addr2info(address_type addr, string &function, string &file, int &line, string &dso)
{
    function = "";
    file = "";
    line = 0;
    dso = "";
    for (map<string, object_file *>::iterator i = object_files.begin(); i != object_files.end(); ++i)
    {
        object_file *of = i->second;
        if (addr >= of->text_avma)
        {
            addr2info_info info;

            info.addr = addr;
            info.obj = of;
            info.found = false;
            for (int pass = 1; pass >= 0; pass--)
            {
                object_subfile *osf = of->subfiles[pass];
                if (osf == NULL) continue;

                info.sub = osf;
                bfd_map_over_sections(osf->abfd, addr2info_section, &info);
                if (info.found)
                {
                    dso = i->first;
                    if (info.function != NULL && info.function[0])
                    {
                        char *demangled = bfd_demangle(osf->abfd, info.function, 0);
                        if (demangled != NULL)
                        {
                            function = demangled;
                            free(demangled);
                        }
                        else
                            function = info.function;
                    }
                    if (info.source != NULL)
                    {
                        file = info.source;
                        line = info.line;
                    }
                    return;
                }
            }
        }
    }
}

/* Takes a full path and returns a short form of it for display by stripping
 * paths
 */
string dg_view_abbrev_file(const string &full)
{
    const char *suffix = strrchr(full.c_str(), '/');
    if (suffix == NULL)
        return full;
    else
    {
        suffix++; /* Skip over the last / */
        return suffix;
    }
}

/* Like dg_view_abbrev_file, but takes the name of a DSO. Currently
 * equivalent.
 */
string dg_view_abbrev_dso(const string &full)
{
    return dg_view_abbrev_file(full);
}

string dg_view_addr2line(address_type addr)
{
    string function, file, dso;
    int line;

    dg_view_addr2info(addr, function, file, line, dso);

    ostringstream label;
    label << hex << showbase << addr << dec << noshowbase;

    if (!function.empty())
        label << " in " << function;

    if (!file.empty())
    {
        label << " (" << dg_view_abbrev_file(file);
        if (line != 0)
            label << ":" << line;
        label << ")";
    }
    else if (!dso.empty())
        label << " (" << dg_view_abbrev_dso(dso) << ")";
    return label.str();
}
