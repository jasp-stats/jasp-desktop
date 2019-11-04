#include "extracontrolsinfo.h"
#include <QMetaProperty>
#include <QQmlListProperty>
#include "qmllistview.h"

void ExtraControlsInfo::read(QMLListView* listView)
{
	_extraControlOptionName = listView->getItemProperty("extraControlOptionName").toString().toStdString();

	QStringList extraControlTitles;
	QString name = listView->getItemProperty("name").toString();
	QList<QVariant> extraControlVariants = listView->getItemProperty("extraControlColumns").toList();
	for (const QVariant& extraControlVariant : extraControlVariants)
	{
		QMap<QString, QVariant> properties;

		QObject* extraControlColumnObject = extraControlVariant.value<QObject*>();
		const QMetaObject *meta = extraControlColumnObject->metaObject();
		int propertyCount = meta->propertyCount();
		for (int i = 0; i < propertyCount; ++i)
		{
			QMetaProperty property = meta->property(i);
			QString key = QString::fromLatin1(property.name());
			QVariant value = property.read(extraControlColumnObject);
			if (key == "purpose")
			{
				if (value.toString() == "nuisance")
					_hasNuisanceControl = true;
			}
			else if (key == "title")
			{
				QString title = value.toString();
				if (!title.isEmpty())
					extraControlTitles.push_back(value.toString());
			}
			else if (key != "properties")
			{
				if (key == "type" && value.toString() == "DropDown")
					// DropDown is an alias to ComboBox
					value = "ComboBox";
				properties[key] = value;
			}
			else
			{
				QMap<QString, QVariant> map = value.toMap();
				QMapIterator<QString, QVariant> it(map);
				while (it.hasNext())
				{
					it.next();
					properties[it.key()] = it.value();
				}

			}
		}

		if (_hasNuisanceControl)
			_optionNuisanceName = properties["name"].toString().toStdString();
		_values.push_back(properties);
	}

	if (_values.length() > 0)
		listView->setProperty("extraControlTitles", extraControlTitles);
}
