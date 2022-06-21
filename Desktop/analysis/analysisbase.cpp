#include "analysisbase.h"
#include "analysisform.h"
#include "log.h"

const std::string AnalysisBase::emptyString;

AnalysisBase::AnalysisBase(QObject* parent) : QObject(parent)
{
}

QQuickItem* AnalysisBase::formItem() const
{
	return _analysisForm;
}

void AnalysisBase::destroyForm()
{
	Log::log() << "Analysis(" << this << ")::destroyForm() called" << std::endl;

	if(_analysisForm)
	{
		Log::log(false) << " it has a AnalysisForm " << _analysisForm << " so let's destroy it" << std::endl;

		_analysisForm->setParent(		nullptr);
		_analysisForm->setParentItem(	nullptr);

		delete _analysisForm;
		_analysisForm = nullptr;

		emit formItemChanged();
	}
	else
		Log::log(false) << " it has no AnalysisForm." << std::endl;
}

void AnalysisBase::createForm(QQuickItem* parentItem)
{
	Log::log() << "Analysis(" << this << ")::createForm() called with parentItem " << parentItem << std::endl;

	setQmlError("");

	if(_analysisForm)
	{
		Log::log() << "It already has a form, so we destroy it." << std::endl;
		if (!parentItem) parentItem = _analysisForm->parentItem();
		destroyForm();
	}

	try
	{
		Log::log()  << std::endl << "Loading QML form from: " << qmlFormPath(false, false) << std::endl;

		QObject * newForm = instantiateQml(QUrl::fromLocalFile(tq(qmlFormPath(false, false))), module(), qmlContext(parentItem));

		Log::log() << "Created a form, got pointer " << newForm << std::endl;

		_analysisForm = qobject_cast<AnalysisForm *>(newForm);

		if(!_analysisForm)
			throw std::logic_error("QML file '" + qmlFormPath(false, false) + "' didn't spawn into AnalysisForm, but into: " + (newForm ? fq(newForm->objectName()) : "null"));

		_analysisForm->setAnalysis(this);
		_analysisForm->setParent(this);
		_analysisForm->setParentItem(parentItem);

		emit formItemChanged();
	}
	catch(std::exception e)
	{
		setQmlError(e.what());
		_analysisForm = nullptr;
	}
}

std::string AnalysisBase::qmlFormPath(bool, bool) const
{
	return module() + "/qml/"  + name();
}

const QString &AnalysisBase::qmlError() const
{
	return _qmlError;
}

void AnalysisBase::setQmlError(const QString &newQmlError)
{
	if (_qmlError == newQmlError)
		return;
	_qmlError = newQmlError;
	emit qmlErrorChanged();
}
