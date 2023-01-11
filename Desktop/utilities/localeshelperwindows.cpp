#include "localeshelperwindows.h"

#ifdef WIN32
#include <stringapiset.h>
#endif

LocalesHelperWindows * LocalesHelperWindows::_singleton = nullptr;

LocalesHelperWindows::LocalesHelperWindows(QObject *parent)
	: QObject{parent}
{
	assert(!_singleton);
	_singleton = this;
}

std::string LocalesHelperWindows::convertCodepagedStrToUtf8(const std::string &raw)
{
#ifdef WIN32
	this is useful:
	int MultiByteToWideChar(
	  [in]            UINT                              CodePage,
	  [in]            DWORD                             dwFlags,
	  [in]            _In_NLS_string_(cbMultiByte)LPCCH lpMultiByteStr,
	  [in]            int                               cbMultiByte,
	  [out, optional] LPWSTR                            lpWideCharStr,
	  [in]            int                               cchWideChar
	);
			as it the code in rbridge_nativeToUtf8
#else
		return raw;
#endif
}


const QStringList &LocalesHelperWindows::locales() const
{
	return _locales;
}

void LocalesHelperWindows::setLocales(const QStringList &newLocales)
{
	if (_locales == newLocales)
		return;
	
	_locales = newLocales;
	emit localesChanged();
}

const QString &LocalesHelperWindows::currentLocale() const
{
	return _currentLocale;
}

void LocalesHelperWindows::setCurrentLocale(const QString &newCurrentLocale)
{
	if (_currentLocale == newCurrentLocale)
		return;
	
	_currentLocale = newCurrentLocale;
	emit currentLocaleChanged();
}

int LocalesHelperWindows::codePage() const
{
	return _codePage;
}

void LocalesHelperWindows::setCodePage(int newCodePage)
{
	if (_codePage == newCodePage)
		return;
	
	_codePage = newCodePage;
	emit codePageChanged();
}
