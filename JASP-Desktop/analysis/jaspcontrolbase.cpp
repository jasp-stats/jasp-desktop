#include "jaspcontrolbase.h"
#include "log.h"
#include "analysisform.h"
#include <QQmlProperty>
#include <QQmlContext>

JASPControlBase::JASPControlBase(QQuickItem *parent) : QQuickItem(parent)
{
	setFlag(ItemIsFocusScope);
	setActiveFocusOnTab(true);
}

void JASPControlBase::setFocusOnTab(bool focus)
{
	if (focus != activeFocusOnTab())
	{
		setActiveFocusOnTab(focus);
		emit focusOnTabChanged();
	}
}

void JASPControlBase::setHasError(bool hasError)
{
	if (section())
		QMetaObject::invokeMethod(section(), "addControlWithError", Qt::DirectConnection, Q_ARG(QVariant, name()), Q_ARG(QVariant, hasError));

	if (form())
		form()->addControlErrorSet(this, hasError);

	if (hasError != _hasError)
	{
		_hasError = hasError;
		emit hasErrorChanged();
	}
}

void JASPControlBase::setHasWarning(bool hasWarning)
{
	if (section())
		QMetaObject::invokeMethod(section(), "addControlWithWarning", Qt::DirectConnection, Q_ARG(QVariant, name()), Q_ARG(QVariant, hasWarning));

	// the call to addControlWithWarning to an expander (section) calls setHasWarning, but this warning does nog have to be added to the form.
	if (form() && (controlType() != JASPControlBase::ControlType::Expander))
		form()->addControlWarningSet(this, hasWarning);

	if (hasWarning != _hasWarning)
	{
		_hasWarning = hasWarning;
		emit hasWarningChanged();
	}
}

void JASPControlBase::setRunOnChangeToChildren(bool change)
{
	if (_childControlsArea)
	{
		QList<JASPControlBase*> childControls = getChildJASPControls(_childControlsArea);
		for (JASPControlBase* childControl : childControls)
			childControl->setRunOnChange(change);
	}
}

void JASPControlBase::setRunOnChange(bool change)
{
	if (change != _runOnChange)
	{
		_runOnChange = change;

		setRunOnChangeToChildren(change);

		emit runOnChangeChanged();
	}
}

void JASPControlBase::componentComplete()
{
	QQuickItem::componentComplete();

	QQmlContext* context = qmlContext(this);
	bool hasContextForm = context->contextProperty("hasContextForm").toBool();

	if (hasContextForm)
		_form = context->contextProperty("form").value<AnalysisForm*>();
	else
	{
		QObject* p = this;
		do
		{
			p = p->parent();
			_form = qobject_cast<AnalysisForm*>(p);
		}
		while (p && !_form);
	}

	_wrapper = JASPControlWrapper::buildJASPControlWrapper(this);

	if (!hasContextForm)
	{
		if (_form)	_form->addControl(this);
		else		_wrapper->setUp();
	}
	else
	{
		bool noDirectSetup = context->contextProperty("noDirectSetup").toBool();
		if (!noDirectSetup)
			_wrapper->setUp();

		QMLListView* listView = nullptr;

		QVariant listViewVar = context->contextProperty("listView");
		if (!listViewVar.isNull())
			listView = dynamic_cast<QMLListView*>(listViewVar.value<QObject*>());
		else
		{
			QVariant tableViewVar = context->contextProperty("tableView");
			if (!tableViewVar.isNull())
			{
				JASPControlBase* tableViewControl = dynamic_cast<JASPControlBase*>(tableViewVar.value<QObject*>());
				if (tableViewControl)
					listView = dynamic_cast<QMLListView*>(tableViewControl->getWrapper());
			}
		}

		if (listView)
		{
			_parentListView = listView->item();
			_parentListViewKey = context->contextProperty(!listViewVar.isNull() ? "rowValue" : "rowIndex").toString();

			listView->addRowControl(_parentListViewKey, _wrapper);

			emit parentListViewChanged();
		}
	}

	if (_background == nullptr && _innerControl != nullptr)
	{
		QVariant innerControlBackround = _innerControl->property("background");
		if (!innerControlBackround.isNull())
			_background = innerControlBackround.value<QQuickItem*>();
	}

	// Set the parentDebug property to children items when the item is completed (and the children are already created)
	if (_debug)
		setParentDebugToChildren(_debug);

	// Also, set the runOnChange property to children items
	if (!_runOnChange)
		setRunOnChangeToChildren(_runOnChange);
}

void JASPControlBase::addControlError(QString message)
{
	if (_form)
		_form->addControlError(this, message, false);
}

void JASPControlBase::addControlErrorTemporary(QString message)
{
	if (_form)
		_form->addControlError(this, message, true);
}

void JASPControlBase::addControlWarning(QString message)
{
	if (_form)
		_form->addControlError(this, message, false, true);
}

void JASPControlBase::addControlWarningTemporary(QString message)
{
	if (_form)
		_form->addControlError(this, message, true, true);
}

void JASPControlBase::clearControlError()
{
	if (_form)
		_form->clearControlError(this);
}

QList<JASPControlBase*> JASPControlBase::getChildJASPControls(QQuickItem* item)
{
	QList<JASPControlBase*> result;

	if (!item)
		return result;

	QList<QQuickItem*> childItems = item->childItems();

	for (QQuickItem* childItem : childItems)
	{
		JASPControlBase* childControl = qobject_cast<JASPControlBase*>(childItem);
		if (childControl)
			result.push_back(childControl);
		else
			result.append(getChildJASPControls(childItem));
	}

	return result;
}

void JASPControlBase::setParentDebug(bool parentDebug)
{
	if (_parentDebug != parentDebug)
	{
		_parentDebug = parentDebug;
		setParentDebugToChildren(_parentDebug || _debug);
		emit parentDebugChanged();
	}
}

void JASPControlBase::setDebug(bool debug)
{
	if (_debug != debug)
	{
		_debug = debug;
		setParentDebugToChildren(_parentDebug || _debug);
		emit debugChanged();
	}
}

void JASPControlBase::setParentDebugToChildren(bool debug)
{
	if (_childControlsArea)
	{
		QList<JASPControlBase*> childControls = getChildJASPControls(_childControlsArea);
		for (JASPControlBase* childControl : childControls)
			childControl->setParentDebug(debug);
	}
}

void JASPControlBase::setRowComponent(QQmlComponent *newRowComponent)
{
	if ( _rowComponents.length() == 0 || newRowComponent != _rowComponents.at(0))
	{
		for (QQmlComponent* rowComponent : _rowComponents)
			delete rowComponent;
		_rowComponents.clear();
		_rowComponents.push_back(newRowComponent);
		emit rowComponentChanged();
	}
}

QQmlListProperty<QQmlComponent> JASPControlBase::rowComponents()
{
	return QQmlListProperty<QQmlComponent>(this, this,
			 &JASPControlBase::appendRowComponent,
			 &JASPControlBase::rowComponentsCount,
			 &JASPControlBase::rowComponent,
			 &JASPControlBase::clearRowComponents);
}

void JASPControlBase::appendRowComponent(QQmlComponent* p) {
	_rowComponents.append(p);
}

int JASPControlBase::rowComponentsCount() const
{
	return _rowComponents.count();
}

QQmlComponent* JASPControlBase::rowComponent(int index) const
{
	return _rowComponents.at(index);
}

void JASPControlBase::clearRowComponents() {
	for (QQmlComponent* rowComponent : _rowComponents)
		delete(rowComponent);
	_rowComponents.clear();
}

void JASPControlBase::appendRowComponent(QQmlListProperty<QQmlComponent>* list, QQmlComponent* p) {
	reinterpret_cast< JASPControlBase* >(list->data)->appendRowComponent(p);
}

void JASPControlBase::clearRowComponents(QQmlListProperty<QQmlComponent>* list) {
	reinterpret_cast< JASPControlBase* >(list->data)->clearRowComponents();
}

QQmlComponent* JASPControlBase::rowComponent(QQmlListProperty<QQmlComponent>* list, int i) {
	return reinterpret_cast< JASPControlBase* >(list->data)->rowComponent(i);
}

int JASPControlBase::rowComponentsCount(QQmlListProperty<QQmlComponent>* list) {
	return reinterpret_cast< JASPControlBase* >(list->data)->rowComponentsCount();
}
