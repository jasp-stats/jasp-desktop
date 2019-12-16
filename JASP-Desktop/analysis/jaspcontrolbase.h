#ifndef JASPCONTROLBASE_H
#define JASPCONTROLBASE_H

#include <QQuickItem>

#include "utilities/qutils.h"

class AnalysisForm;
class JASPControlWrapper;

class JASPControlBase : public QQuickItem
{
	Q_OBJECT

private:
	Q_PROPERTY( ControlType		controlType			READ controlType		WRITE setControlType											)
	Q_PROPERTY( QString			name				READ name				WRITE setName				NOTIFY nameChanged					)
	Q_PROPERTY( bool			isBound				READ isBound			WRITE setIsBound			NOTIFY isBoundChanged				)
	Q_PROPERTY( bool			focusOnTab			READ focusOnTab			WRITE setFocusOnTab			NOTIFY focusOnTabChanged			)
	Q_PROPERTY( bool			hasError			READ hasError			WRITE setHasError			NOTIFY hasErrorChanged				)
	Q_PROPERTY( bool			hasWarning			READ hasWarning			WRITE setHasWarning			NOTIFY hasWarningChanged			)
	Q_PROPERTY( QVariant		childControlsArea	READ childControlsArea	WRITE setChildControlsArea										)
	Q_PROPERTY( QQuickItem*		section				READ section			WRITE setSection												)
	Q_PROPERTY( QQmlListProperty<QQmlComponent> rowComponents READ rowComponents)

public:
	enum ControlType {
		  JASPControl
		, Expander
		, CheckBox
		, Switch
		, TextField
		, RadioButton
		, RadioButtonGroup
		, VariablesListView
		, ComboBox
		, RepeatedMeasuresFactorsList
		, InputListView
		, TableView
		, Slider
		, TextArea
		, Button
		, FactorsForm
		, ComponentsList
	};

	Q_ENUM(ControlType)

	JASPControlBase(QQuickItem *parent = nullptr);

	ControlType		controlType()			const	{ return _controlType;			}
	const QString&	name()					const	{ return _name;					}
	bool			isBound()				const	{ return _isBound;				}
	bool			hasError()				const	{ return _hasError;				}
	bool			hasWarning()			const	{ return _hasWarning;			}
	bool			focusOnTab()			const	{ return activeFocusOnTab();	}
	AnalysisForm*	form()					const	{ return _form;					}
	const QVariant&	childControlsArea()		const	{ return _childControlsArea;	}
	QQuickItem*		section()				const	{ return _section;				}

	void	setControlType(ControlType controlType)				{ _controlType = controlType; }
	void	setChildControlsArea(QVariant childControlsArea)	{ _childControlsArea = childControlsArea; }
	void	setSection(QQuickItem* section)						{ _section = section; }
	void	setFocusOnTab(bool focus);
	void	setHasError(bool hasError);
	void	setHasWarning(bool hasWarning);

	GENERIC_SET_FUNCTION(Name		, _name			, nameChanged		, QString	)
	GENERIC_SET_FUNCTION(IsBound	, _isBound		, isBoundChanged	, bool		)

	JASPControlWrapper*				getWrapper() const { return _wrapper; }

	QQmlListProperty<QQmlComponent>	rowComponents();
	void							appendRowComponent(QQmlComponent *);
	int								rowComponentsCount() const;
	QQmlComponent*					rowComponent(int) const;
	void							clearRowComponents();

	QVector<QQmlComponent*> &		getRowComponents() { return _rowComponents; }

	Q_INVOKABLE	void				addControlError(QString message);
	Q_INVOKABLE void				addControlErrorTemporary(QString message);
	Q_INVOKABLE	void				addControlWarning(QString message);
	Q_INVOKABLE void				addControlWarningTemporary(QString message);
	Q_INVOKABLE void				clearControlError();

signals:
	void nameChanged();
	void isBoundChanged();
	void hasErrorChanged();
	void hasWarningChanged();
	void focusOnTabChanged();

protected:
	void componentComplete() override;

	void _setType();

protected:
	ControlType			_controlType;
	QString				_name;
	bool				_isBound		= true;
	bool				_hasError		= false;
	bool				_hasWarning		= false;
	JASPControlWrapper*	_wrapper		= nullptr;
	AnalysisForm*		_form			= nullptr;
	QVariant			_childControlsArea;
	QQuickItem*			_section		= nullptr;

	static void				appendRowComponent(QQmlListProperty<QQmlComponent>*, QQmlComponent*);
	static int				rowComponentsCount(QQmlListProperty<QQmlComponent>*);
	static QQmlComponent*	rowComponent(QQmlListProperty<QQmlComponent>*, int);
	static void				clearRowComponents(QQmlListProperty<QQmlComponent>*);

	QVector<QQmlComponent*>	_rowComponents;
};


#endif // JASPCONTROLBASE_H
