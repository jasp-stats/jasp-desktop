#ifndef LABEL_H
#define LABEL_H

#include <boost/interprocess/managed_shared_memory.hpp>
#include <boost/interprocess/segment_manager.hpp>
#include <boost/container/string.hpp>

class Label;

typedef boost::interprocess::allocator<char, boost::interprocess::managed_shared_memory::segment_manager> CharAllocator;
typedef boost::container::basic_string<char, std::char_traits<char>, CharAllocator> String;
typedef boost::interprocess::allocator<String, boost::interprocess::managed_shared_memory::segment_manager> StringAllocator;

class Label
{
public:
	Label(boost::interprocess::managed_shared_memory *mem, std::string value);
	Label(boost::interprocess::managed_shared_memory *mem, int value);

	std::string text() const;
	bool hasIntValue() const;
	int value() const;
	Label& operator=(const Label &label);

private:

	boost::interprocess::managed_shared_memory *_mem;
	bool _hasIntValue;
	int _intValue;
	String _stringValue;
};

#endif // LABEL_H
