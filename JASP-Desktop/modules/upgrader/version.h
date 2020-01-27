#ifndef MODULEVERSION_H
#define MODULEVERSION_H

#include <string>
#include <stdexcept>

namespace Modules
{


class Version
{
public:
	struct encodingError  : public std::runtime_error
	{
		encodingError(std::string versionStr) : std::runtime_error("Module version not understood: " + versionStr) {}
		const char* what() const noexcept override;
	};

	Version(std::string version);
	Version(unsigned int major = 0, unsigned int minor = 0, unsigned int release = 0, unsigned int fourth = 0) : _major(major), _minor(minor), _release(release), _fourth(fourth) {}

	std::string toString() const;

	unsigned int major()	const { return _major; }
	unsigned int minor()	const { return _minor; }
	unsigned int release()	const { return _release; }
	unsigned int fourth()	const { return _fourth; }

	void		swap(Version &other );

	Version &	operator =	(const Version & other);
	bool		operator <	(const Version & other) const;
	bool		operator <=	(const Version & other) const;
	bool		operator >=	(const Version & other) const;
	bool		operator >	(const Version & other) const;
	bool		operator ==	(const Version & other) const;
	bool		operator !=	(const Version & other) const;


private:
	unsigned int	_major		= 0,
					_minor		= 0,
					_release	= 0,
					_fourth		= 0; //This is just here to be able to parse older files...
};
}

#endif // MODULEVERSION_H
