#ifndef EXTRACONTROSINFO_H
#define EXTRACONTROSINFO_H

#include <QVariant>
#include <QQmlListProperty>

class QMLListView;

class ExtraControlsInfo
{
public:

	ExtraControlsInfo() {}
	void read(QMLListView* listView);

	const QVector<QMap<QString, QVariant> >& values()		{ return _values; }
	bool hasNuisanceControl()								{ return _hasNuisanceControl; }
	const std::string& optionNuisanceName()					{ return _optionNuisanceName; }
	const std::string& extraControlOptionName()				{ return _extraControlOptionName; }

private:
	std::string							_optionNuisanceName;
	std::string							_extraControlOptionName;
	bool								_hasNuisanceControl = false;
	QVector<QMap<QString, QVariant> >	_values;

};

#endif // EXTRACONTROSINFO_H
