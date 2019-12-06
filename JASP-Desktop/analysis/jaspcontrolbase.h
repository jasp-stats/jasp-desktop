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
	Q_PROPERTY( bool			focusOnTab			READ focusOnTab			WRITE setFocusOnTab			NOTIFY focusOnTabChanged				)
	Q_PROPERTY( QVariant		childControlsArea	READ childControlsArea	WRITE setChildControlsArea										)
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
	bool			focusOnTab()			const	{ return activeFocusOnTab();	}
	AnalysisForm*	form()					const	{ return _form;					}
	const QVariant&	childControlsArea()		const	{ return _childControlsArea;	}

	void	setControlType(ControlType controlType)				{ _controlType = controlType; }
	void	setChildControlsArea(QVariant childControlsArea)	{ _childControlsArea = childControlsArea; }
	void	setFocusOnTab(bool focus);

	GENERIC_SET_FUNCTION(Name		, _name			, nameChanged		, QString	)
	GENERIC_SET_FUNCTION(IsBound	, _isBound		, isBoundChanged	, bool		)

	JASPControlWrapper*				getWrapper() const { return _wrapper; }

	QQmlListProperty<QQmlComponent>	rowComponents();
	void							appendRowComponent(QQmlComponent *);
	int								rowComponentsCount() const;
	QQmlComponent*					rowComponent(int) const;
	void							clearRowComponents();

	QVector<QQmlComponent*> &		getRowComponents() { return _rowComponents; }

signals:
	void nameChanged();
	void isBoundChanged();
	void focusOnTabChanged();

protected:
	void componentComplete() override;

	void _setType();

protected:
	ControlType			_controlType;
	QString				_name;
	bool				_isBound		= true;
	JASPControlWrapper*	_wrapper		= nullptr;
	AnalysisForm*		_form			= nullptr;
	QVariant			_childControlsArea;

	static void				appendRowComponent(QQmlListProperty<QQmlComponent>*, QQmlComponent*);
	static int				rowComponentsCount(QQmlListProperty<QQmlComponent>*);
	static QQmlComponent*	rowComponent(QQmlListProperty<QQmlComponent>*, int);
	static void				clearRowComponents(QQmlListProperty<QQmlComponent>*);

	QVector<QQmlComponent*>	_rowComponents;
};


#endif // JASPCONTROLBASE_H
