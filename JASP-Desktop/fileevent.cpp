
#include "fileevent.h"

#include <QTimer>

FileEvent::FileEvent(QObject *parent)
	: QObject(parent)
{
	_operation = FileEvent::FileOpen;
	_readOnly = false;
	_chainedTo = NULL;
}

void FileEvent::setOperation(FileEvent::FileMode fileMode)
{
	_operation = fileMode;
}

void FileEvent::setPath(const QString &path)
{
	_path = path;
}

void FileEvent::setReadOnly()
{
	_readOnly = true;
}

FileEvent::FileMode FileEvent::operation() const
{
	return _operation;
}

const QString &FileEvent::path() const
{
	return _path;
}

bool FileEvent::isReadOnly() const
{
	return _readOnly;
}

void FileEvent::setComplete(bool success, const QString &message)
{
	_isComplete = true;
	_success = success;
	_message = message;

	emit completed(this);
}

bool FileEvent::isCompleted() const
{
	return _isComplete;
}

bool FileEvent::successful() const
{
	return _success;
}

const QString &FileEvent::message() const
{
	return _message;
}

void FileEvent::chain(FileEvent *event)
{
	_chainedTo = event;
	connect(event, SIGNAL(completed(FileEvent*)), this, SLOT(chainedComplete(FileEvent*)));
}

void FileEvent::emitComplete()
{
	emit completed(this);
}

void FileEvent::chainedComplete(FileEvent *event)
{
	setComplete(event->successful(), event->message());
}

