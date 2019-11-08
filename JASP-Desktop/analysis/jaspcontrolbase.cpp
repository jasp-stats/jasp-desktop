#include "jaspcontrolbase.h"
#include "log.h"
#include "analysisform.h"
#include <QQmlProperty>
#include <QQmlContext>

JASPControlBase::JASPControlBase(QQuickItem *parent)
{
	setFlag(ItemIsFocusScope);
	setActiveFocusOnTab(true);
}

void JASPControlBase::componentComplete()
{
	QQuickItem::componentComplete();

	QObject* p = this;
	do
	{
		p = p->parent();
		_form = qobject_cast<AnalysisForm*>(p);
	}
	while (p && !_form);

	_wrapper = JASPControlWrapper::buildJASPControlWrapper(this);

	QQmlContext* context = qmlContext(this);
	bool noSetup = context->contextProperty("noSetup").toBool();
	bool isRowComponent = context->contextProperty("isRowComponent").toBool();
	if (isRowComponent)
		_form = context->contextProperty("form").value<AnalysisForm*>();

	if (!isRowComponent && _form && _isBound)
		_form->addControl(this);
	else if (!noSetup)
		_wrapper->setUp();
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
