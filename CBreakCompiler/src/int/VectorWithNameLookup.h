// Copyright (c) Luke Lenhart.  See LICENSE.txt for details.

#pragma once
#include <vector>
#include <unordered_map>
#include <string>
#include <limits>

namespace CBreakCompilerInternal
{
    static const size_t VectorWithNameLookup_NotFoundIndex = std::numeric_limits<size_t>::max();

    template<typename T>
    class VectorWithNameLookup
    {
    public:
        T* TryFindName(const std::string &name)
        {
            auto iter = nameToIndex.find(name);
            if (iter == nameToIndex.end())
                return nullptr;

            return &items[iter->second];
        }

        const T* TryFindName(const std::string &name) const
        {
            auto iter = nameToIndex.find(name);
            if (iter == nameToIndex.end())
                return nullptr;

            return &items[iter->second];
        }

        size_t TryFindIndexForName(const std::string &name) const
        {
            auto iter = nameToIndex.find(name);
            if (iter == nameToIndex.end())
                return VectorWithNameLookup_NotFoundIndex;

            return iter->second;
        }

        T& operator[](size_t index)
        {
            return items[index];
        }

        const T& operator[](size_t index) const
        {
            return items[index];
        }

        T& back()
        {
            return items.back();
        }

        const T& back() const
        {
            return items.back();
        }

        typename std::vector<T>::iterator begin()
        {
            return items.begin();
        }

        typename std::vector<T>::const_iterator begin() const
        {
            return items.begin();
        }

        typename std::vector<T>::iterator end()
        {
            return items.end();
        }

        typename std::vector<T>::const_iterator end() const
        {
            return items.end();
        }

        size_t size() const
        {
            return items.size();
        }

        bool empty() const
        {
            return items.empty();
        }

        template <typename AddType>
        void Add(const std::string &name, AddType &&add)
        {
            nameToIndex.emplace(name, items.size());
            items.emplace_back(add);
        }

        T& Add(const std::string &name)
        {
            nameToIndex.emplace(name, items.size());
            items.emplace_back();
            return items.back();
        }

    private:
        std::vector<T> items;
        std::unordered_map<std::string, size_t> nameToIndex;
    };
}
