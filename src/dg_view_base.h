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

#ifndef DG_VIEW_BASE_H
#define DG_VIEW_BASE_H

#include <stdint.h>
#include <vector>
#include <string>
#include <map>
#include <cstddef>

#include "dg_view_options.h"

typedef uint64_t address_type;
typedef uint64_t iseq_type;

/* Abstract base class, which holds data about an input file. It is subclassed
 * based on the word size of the file.
 */
class dg_view_base
{
public:
    class iterator;

private:
    /* Abstract base class for the data in an iterator. This is separated
     * out from the iterator class itself so that the iterator class can
     * be non-virtual.
     */

    class iterator_data_base
    {
    protected:
        friend class dg_view_base::iterator;

        /* Creates an identical copy in new memory */
        virtual iterator_data_base *clone() const = 0;

        /* Moves on to the next access */
        virtual void increment() = 0;

    public:
        virtual ~iterator_data_base() {}

        virtual address_type get_addr() const = 0;
        virtual iseq_type get_iseq() const = 0;
        virtual size_t get_size() const = 0; /* Size of the access */
        virtual uint8_t get_dir() const = 0; /* One of the DG_ACC_* tokens */
        virtual std::vector<address_type> get_stack() const = 0;

        /* Information about the memory block accessed */
        virtual address_type get_mem_addr() const = 0;
        virtual address_type get_mem_size() const = 0; /* 0 if no information available */
        virtual std::vector<address_type> get_mem_stack() const = 0;
        virtual std::string get_mem_label() const = 0;

        virtual bool operator==(const iterator_data_base &b) const = 0;
    };

public:
    /* Iterator over the data accesses. This is a wrapper class that
     * wraps an instance of a subclass of iterator_data.
     */
    class iterator
    {
    public:
        iterator_data_base *data; /* Actually a pointer to a subclass */

        iterator() : data(NULL) {}
        iterator(const iterator &it) : data(NULL)
        {
            if (it.data != NULL)
                data = it.data->clone();
        }

        explicit iterator(iterator_data_base *data) : data(data) {}

        ~iterator() { delete data; }

        iterator_data_base *operator->() const
        {
            return data;
        }

        iterator &operator=(const iterator &it)
        {
            delete data;
            if (it.data != NULL)
            {
                data = it.data->clone();
            }
            else
            {
                data = NULL;
            }
            return *this;
        }

        iterator &operator++()
        {
            data->increment();
            return *this;
        }

        iterator operator++(int)
        {
            iterator old(*this);
            data->increment();
            return old;
        }

        bool operator==(const iterator &it) const
        {
            if (data == NULL && it.data == NULL)
            {
                return true;
            }
            else if (data != NULL && it.data != NULL)
            {
                return *data == *it.data;
            }
            else
            {
                return false;
            }
        }

        bool operator!=(const iterator &it) const
        {
            return !(*this == it);
        }
    };

    typedef iterator const_iterator;

    typedef std::map<address_type, size_t>::const_iterator forward_page_map_iterator;

protected:
    friend dg_view_base *dg_view_load(const std::string &filename, const dg_view_options &options);

public:
    virtual const_iterator begin() const = 0;
    virtual const_iterator end() const = 0;
    virtual void get_ranges(address_type &addr_min, address_type &addr_max, iseq_type &iseq_min, iseq_type &iseq_max) const = 0;

    virtual forward_page_map_iterator page_map_begin() const = 0;
    virtual forward_page_map_iterator page_map_end() const = 0;

    /* Takes a compressed address and maps it to an original one */
    virtual address_type revmap_addr(std::size_t addr) const = 0;
    /* Maps original address to compressed address */
    virtual std::size_t remap_address(address_type a) const = 0;

    /* ratio is the ratio of address scale to iseq scale: a large value for ratio
     * increases the importance of the address in the match.
     *
     * Returns the best score and best index for the block. If there were no
     * usable addresses, returns score of HUGE_VAL.
     */
    virtual const_iterator nearest_access(double addr, double iseq, double ratio) const = 0;
};

#endif /* DG_VIEW_BASE_H */
