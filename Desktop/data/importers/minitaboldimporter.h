#ifndef MINITABOLDIMPORTER_H
#define MINITABOLDIMPORTER_H

#include "importer.h"
#include "stringutils.h"
#include "timers.h"
#include <string>

///
/// Uses the mtw-ffi library to import the classic Minitab files: worksheets
/// (.mtw, both the OLE-era formats and the legacy pre-1995 ones), projects
/// (.mpj) and portable worksheets (.mtp). The modern .mwx/.mpx workbooks are
/// read by MinitabImporter instead.
class MinitabOldImporter : public Importer
{

public:
	MinitabOldImporter(std::string ext = "mtw") : Importer(), _ext(stringUtils::toLower(ext))
	{
		if(_ext[0] == '.')		_ext = _ext.substr(1);
	}
	~MinitabOldImporter() override;

	static bool extSupported(const std::string & ext);
	static stringset extsSupported();

protected:
	ImportDataSet *	loadFile(const std::string &locator, std::function<void(int)> progressCallback)	override;

	std::string		_ext;

private:
	JASPTIMER_CLASS(MinitabOldImporter);
};

#endif // MINITABOLDIMPORTER_H
