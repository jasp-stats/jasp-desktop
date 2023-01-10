#include "localeshelperwindows.h"
#include <QString>
#include <QTextStre

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
		return QString::fromLocal8Bit(raw.c_str()).toStdString()
#endif
}

