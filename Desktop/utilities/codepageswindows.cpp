#include "codepageswindows.h"
#include "log.h"
#include "gui/preferencesmodel.h"

#ifdef WIN32
#include <Windows.h>

int EnumCodePagesProcJASP(
  LPWSTR lpCodePageString
)
{
	CodePagesWindows::addCodepageIdentifier(std::wstring(lpCodePageString));
	return 1;
};

#endif

CodePagesWindows * CodePagesWindows::_singleton = nullptr;



CodePagesWindows::CodePagesWindows(QObject *parent)
	: QObject{parent}
{
	assert(!_singleton);
	_singleton = this;


	
	_timer.setInterval(50);
	_timer.callOnTimeout(this, &CodePagesWindows::codePageIDsChanged);
	
#ifdef WIN32
	//Request a list of codepages from OS:
	if(!EnumSystemCodePagesW(&EnumCodePagesProcJASP, CP_SUPPORTED))
		setError(true); //Shouldnt occur anyhow but if it does we should disable the combobox etc
#endif
}

std::string CodePagesWindows::convertCodePageStrToUtf8(const std::string &raw)
{
#ifdef WIN32
	assert(_singleton);

	if(PreferencesModel::prefs()->windowsChosenCodePage() < 0)
		return raw;

	uint codepage(PreferencesModel::prefs()->windowsChosenCodePage());

	int size = MultiByteToWideChar(codepage, MB_COMPOSITE, raw.c_str(), raw.length(), nullptr, 0);
	std::wstring utf16_str(size, '\0');

	MultiByteToWideChar(codepage, MB_COMPOSITE, raw.c_str(), raw.length(), &utf16_str[0], size);

	int utf8_size = WideCharToMultiByte(CP_UTF8, 0, utf16_str.c_str(), utf16_str.length(), nullptr, 0, nullptr, nullptr);
	std::string utf8_str(utf8_size, '\0');

	WideCharToMultiByte(CP_UTF8, 0, utf16_str.c_str(), utf16_str.length(), &utf8_str[0], utf8_size,	nullptr, nullptr);

	return utf8_str;
#else
	return raw;
#endif
}

void CodePagesWindows::addCodepageIdentifier(const std::wstring &codePageIdentifier)
{
#ifdef WIN32
	assert(_singleton);

	QString converted	= QString::fromStdWString(codePageIdentifier);
	bool	worked		= false;
	int		codepage	= converted.toInt(&worked);

	if(!worked)
	{
		Log::log() << "addCodepageIdentifier got unconvertible codepage: " << converted.toStdString() << std::endl;
		_singleton->setError(true);
		return;
	}

	//Now we get the userfriendly name:
	CPINFOEXW info;

	if(!GetCPInfoExW(codepage, 0, &info))
	{
		_singleton->setError(true);
		return;
	}

	QString fullName = QString::fromStdWString(info.CodePageName);

	Log::log() << "addCodepageIdentifier: " << codepage << "\t" << fullName << std::endl;

	_singleton->_codePageIDs.push_back(fullName);
	_singleton->_codePageIDMap[fullName] = codepage;

	if(codepage == PreferencesModel::prefs()->windowsChosenCodePage())
	{
				_singleton->_timer.stop();
		emit	_singleton->codePageIDsChanged(); //Send a signal now to update the combobox to contain at least the one we previously selected:
				_singleton->setCodePageID(fullName);
	}
	else
		//Because we get called from a callback we do not know whether more are coming after this or not we put a resetting delay between additions and sending CPIDsChanged
		_singleton->_timer.start();
#else
	throw std::runtime_exception("void CodePagesWindows::addCodepageIdentifier(const std::wstring &codePageIdentifier) doesnt make *ANY* sense on not-windows...");
#endif
}

const QStringList &CodePagesWindows::codePageIDs() const
{
	return _codePageIDs;
}

const QString & CodePagesWindows::codePageID() const
{
	return _codePageID;
}

void CodePagesWindows::setCodePageID(const QString & codePageID)
{
	if (_codePageID == codePageID)
		return;
	
	_codePageID = codePageID;

	PreferencesModel::prefs()->setWindowsChosenCodePage(_singleton->_codePageIDMap.count(codePageID) ? _singleton->_codePageIDMap[_codePageID] : -1);

	emit codePageIDChanged();
}

bool CodePagesWindows::error() const
{
	return _error;
}

void CodePagesWindows::setError(bool newError)
{
	if (_error == newError)
		return;
	_error = newError;
	emit errorChanged();
}
