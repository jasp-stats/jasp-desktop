#ifndef EMPTYVALUES_H
#define EMPTYVALUES_H

#include <QObject>
#include "utils.h"
#include "json/value.h"

class EmptyValues : public QObject
{
	Q_OBJECT
public:
	explicit					EmptyValues(EmptyValues * parent = nullptr, QObject * qparent = nullptr);
								~EmptyValues();
								
			void				resetEmptyValues();

			void				fromJson(				const Json::Value	& json);
			Json::Value			toJson() const;
			
            bool				isEmptyValue(const std::string & data)				const;
            bool				isEmptyValue(double				data)           	const;
			
	const	stringset		&	emptyStrings()										const;
	const	stringset		&	emptyStringsColumnModel()							const;
	const	doubleset		&	emptyDoubles()										const;
			bool				hasEmptyValues()									const;
			void				setHasCustomEmptyValues(bool hasThem);
		    void				setEmptyValues(const stringset	& values);
			void				setEmptyValues(const stringset	& values, bool custom);
			
			static	void		setDisplayString(const std::string & str)	{ _displayString = str;}
	static	std::string		&	displayString()								{ return _displayString; }

	static	const int			missingValueInteger;
	static	const double		missingValueDouble;
	
signals:
			void				emptyValuesChanged();

private:
	
	
private:
	static	std::string			_displayString;
			EmptyValues		*	_parent					= nullptr;
			stringset			_emptyStrings;
			doubleset			_emptyDoubles;
			bool				_hasEmptyValues			= false;
};

#endif // EMPTYVALUES_H
