#include "codepageswindows.h"

#ifdef WIN32
#include <Windows.h>

bool EnumCodePagesProcJASP(
  LPWSTR lpCodePageString
)
{
	CodePagesWindows::addCodepageIdentifier(std::wstring(&lpCodePageString));
	return true;
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
	EnumSystemCodePagesW(EnumCodePagesProcJASP, CP_SUPPORTED);
#endif
}

void CodePagesWindows::startTimer()
{
	_timer.start();	
}


std::string CodePagesWindows::convertCodePageStrToUtf8(const std::string &raw)
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

void CodePagesWindows::addCodepageIdentifier(const std::wstring &codePageIdentifier)
{
#ifdef WIN32
	QString converted = QString::fromStdWString(codePageIdentifier);
	Log::log() << "addCodepageIdentifier: " << converted << std::endl;
	_singleton->_codePageIDs.add(converted);
	//Because we get called from a callback we do not know whether this will end after this or not. To minimize signals going off a small delay is introduced through the timer.
	_singleton->startTimer();
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
	emit codePageIDChanged();
}

void CodePagesWindows::determineCodePageNumber()
{
		
}

int CodePagesWindows::codePage() const
{
	return _codePage;
}

void CodePagesWindows::setCodePage(int newCodePage)
{
	if (_codePage == newCodePage)
		return;
	
	_codePage = newCodePage;
	emit codePageChanged();
}

