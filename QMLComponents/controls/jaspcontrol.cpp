#include "jaspcontrol.h"
#include "jasplistcontrol.h"
#include "log.h"
#include "analysisform.h"
#include "jasptheme.h"
#include <QQmlProperty>
#include <QQmlContext>
#include <QTimer>
#include <QQuickWindow>

const QStringList JASPControl::_optionReservedNames = {"data", "version"};

QMap<QQmlEngine*, QQmlComponent*> JASPControl::_mouseAreaComponentMap;
QByteArray JASPControl::_mouseAreaDef = "\
	import QtQuick\n\
	import QtQuick.Controls\n\
	MouseArea {\n\
	z:					5\n\
	anchors.fill:		parent\n\
	acceptedButtons:	Qt.NoButton\n\
	ToolTip.timeout:	jaspTheme.toolTipTimeout\n\
	ToolTip.delay:		jaspTheme.toolTipDelay\n\
	ToolTip.text:		parent ? parent.toolTip : ''\n\
	ToolTip.visible:	ToolTip.text && containsMouse\n\
}";

QQmlComponent* JASPControl::getMouseAreaComponent(QQmlEngine* engine)
{
	QQmlComponent* result = _mouseAreaComponentMap[engine];
	if (result == nullptr)
	{
		result = new QQmlComponent(engine);
		result->setData(_mouseAreaDef, QUrl());
		_mouseAreaComponentMap[engine] = result;
	}

	return result;
}

JASPControl::JASPControl(QQuickItem *parent) : QQuickItem(parent)
{
	setFlag(ItemIsFocusScope);
	setActiveFocusOnTab(true);

	connect(this, &JASPControl::titleChanged,			this, &JASPControl::helpMDChanged);
	connect(this, &JASPControl::infoChanged,			this, &JASPControl::helpMDChanged);
	connect(this, &JASPControl::visibleChanged,			this, &JASPControl::helpMDChanged);
	connect(this, &JASPControl::visibleChildrenChanged,	this, &JASPControl::helpMDChanged);
	connect(this, &JASPControl::backgroundChanged,		[this] () { if (!_focusIndicator)		setFocusIndicator(_background); });
	connect(this, &JASPControl::infoChanged,			[this] () { if (_toolTip.isEmpty())	setToolTip(info());					});
	connect(this, &JASPControl::toolTipChanged,			[this] () { setShouldStealHover(!_toolTip.isEmpty());					});
	connect(this, &JASPControl::hasErrorChanged,		this, &JASPControl::_hightlightBorder);
	connect(this, &JASPControl::hasWarningChanged,		this, &JASPControl::_hightlightBorder);
	connect(this, &JASPControl::isDependencyChanged,	this, &JASPControl::_hightlightBorder);
	connect(this, &JASPControl::activeFocusChanged,		this, &JASPControl::_hightlightBorder);
	//connect(this, &JASPControl::implicitWidthChanged,	[this] () { setWidth(implicitWidth());		if (_preferredWidthBinding) setPreferredWidth(int(implicitWidth()), true);		});
	//connect(this, &JASPControl::implicitHeightChanged,	[this] () { setHeight(implicitHeight());	if (_preferredHeightBinding) setPreferredHeight(int(implicitHeight()), true);	});
	connect(this, &JASPControl::indentChanged,			[this] () { QQmlProperty(this, "Layout.leftMargin", qmlContext(this)).write( (indent() && JaspTheme::currentTheme()) ? JaspTheme::currentTheme()->indentationLength() : 0); });
	connect(this, &JASPControl::debugChanged,			[this] () { _setBackgroundColor(); _setVisible(); } );
	connect(this, &JASPControl::parentDebugChanged,		[this] () { _setBackgroundColor(); _setVisible(); } );
	connect(this, &JASPControl::boundValueChanged,		this, &JASPControl::_resetBindingValue);
	connect(this, &JASPControl::activeFocusChanged,		this, &JASPControl::_setFocus);
	connect(this, &JASPControl::activeFocusChanged,		this, &JASPControl::_notifyFormOfActiveFocus);
}

JASPControl::~JASPControl()
{
	//first we disconnect the children because reconnectWithYourChildren connected them to the parent
	//These might get triggered during the destructor of QQuickItem and then crash jasp...
	for (JASPControl* child : getChildJASPControls(_childControlsArea))
		child->disconnect();

	disconnect();
}

void JASPControl::setFocusOnTab(bool focus)
{
	if (focus != activeFocusOnTab())
	{
		setActiveFocusOnTab(focus);
		emit focusOnTabChanged();
	}
}

void JASPControl::setInnerControl(QQuickItem* control)
{
	if (control != _innerControl)
	{
		_innerControl = control;
		if (_innerControl && !qobject_cast<JASPControl*>(_innerControl))
		{
			connect(_innerControl, &QQuickItem::activeFocusChanged, this, &JASPControl::_hightlightBorder);
			//capture focus reason
			control->installEventFilter(this);
		}

		emit innerControlChanged();
	}
}

void JASPControl::setPreferredHeight(int preferredHeight, bool isBinding)
{
	if (!isBinding) _preferredHeightBinding = false; // unbind with implicitHeight

	if (preferredHeight != _preferredHeight)
	{
		_preferredHeight = preferredHeight;
		//setProperty("Layout.preferredHeight", preferredHeight); // This does not work...
		bool success = QQmlProperty(this, "Layout.preferredHeight", qmlContext(this)).write(preferredHeight);

		emit preferredHeightChanged();
	}
}

void JASPControl::setPreferredWidth(int preferredWidth, bool isBinding)
{
	if (!isBinding) _preferredWidthBinding = false; // unbind with implicitWidth

	if (preferredWidth != _preferredWidth)
	{
		_preferredWidth = preferredWidth;
		QQmlProperty(this, "Layout.preferredWidth", qmlContext(this)).write(preferredWidth);

		emit preferredWidthChanged();
	}
}

void JASPControl::_setBackgroundColor()
{
	if (background() && JaspTheme::currentTheme() && (debug() || parentDebug()))
		background()->setProperty("color", JaspTheme::currentTheme()->debugBackgroundColor());
}

void JASPControl::_setVisible()
{
	bool isDebug = false;
#ifdef JASP_DEBUG
	isDebug = true;
#endif
	if (!isDebug && (debug() || parentDebug()))
		setVisible(false);
}

void JASPControl::_resetBindingValue()
{
	// If a control gets a value from a JASP file, this value may differ from its default value sets by a QML binding:
	// this QML binding may then change the value during the initialization of the form.
	// In this case, restore the original value.
	if (isBound() && hasUserInteractiveValue() && initializedWithValue() && form() && !form()->initialized())
		boundControl()->resetBoundValue();
}

void JASPControl::setHasError(bool hasError)
{
	if (hasError != _hasError)
	{
		_hasError = hasError;
		emit hasErrorChanged();
	}
}

void JASPControl::setHasWarning(bool hasWarning)
{
	if (hasWarning != _hasWarning)
	{
		_hasWarning = hasWarning;
		emit hasWarningChanged();
	}
}

void JASPControl::componentComplete()
{
	QQuickItem::componentComplete();
	_setBackgroundColor();
	_setVisible();

	connect(this, &JASPControl::initializedChanged, this, &JASPControl::_checkControlName);

	if (_useControlMouseArea)
	{
		QQmlComponent* comp = getMouseAreaComponent(qmlEngine(this));
		QVariantMap props = { {"hoverEnabled", shouldStealHover()}, {"cursorShape", cursorShape()} };

		_mouseAreaObj = qobject_cast<QQuickItem*>(comp->createWithInitialProperties(props, qmlContext(this)));
		if (_mouseAreaObj)
		{
			_mouseAreaObj->setParentItem(this);
			QQuickItem::connect(_mouseAreaObj, SIGNAL(hoveredChanged()), this,  SLOT(_hoveredChangedSlot()) );
		}
		else
			Log::log() << "Cannot create a Mouse Area!!!" << std::endl;
	}

	QQmlContext* context = qmlContext(this);
	bool isDynamic = context->contextProperty("isDynamic").toBool();
	_form = context->contextProperty("form").value<AnalysisForm*>();

	if (!_form)
	{
		// The control is used outside of a form, typically this is used by the Desktop application direclty
		// Just call its setup function, and it is then already initialized.
		setUp();
		setInitialized();
	}
	else if (!isDynamic)
		// For statically build controls in a form, the form self will setup the controls when the form is completely loaded
		// (by calling the AnalysisForm::setAnalysisUp function).
		_form->addControl(this);
	else
	{
		// The control is created dynamically, this is the case for row components.
		// They are created either from a ListView (or a TableView): when all terms of the ListView are set, the row components are created, and then initialized (via rhe ListModel::setUpRowControls function).
		// Here the parent ListView and the key for this control is stored.
		setUp();

		JASPListControl* listView = nullptr;

		QVariant listViewVar = context->contextProperty("listView");
		if (!listViewVar.isNull())
			listView = listViewVar.value<JASPListControl*>();
		else
		{
			QVariant tableViewVar = context->contextProperty("tableView");
			if (!tableViewVar.isNull())
				listView = tableViewVar.value<JASPListControl*>();
		}

		if (listView && listView != this)
		{
			_parentListView = listView;

			if (!listViewVar.isNull())
			{
				_parentListViewKey = context->contextProperty("rowValue").toString();
				connect(listView->model(), &ListModel::oneTermChanged, this, &JASPControl::parentListViewKeyChanged);
			}
			else
				_parentListViewKey = context->contextProperty("rowIndex").toString();

			listView->addRowControl(_parentListViewKey, this);

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

	if (_form)
		connect(this, &JASPControl::boundValueChanged, _form, &AnalysisForm::boundValueChangedHandler);
}

void JASPControl::setCursorShape(int shape)
{
	_cursorShape = shape;

	if (_mouseAreaObj)
		_mouseAreaObj->setProperty("cursorShape", shape);
}

void JASPControl::addControlError(QString message)
{
	if (_form && message.size())
		_form->addControlError(this, message, false);
}

void JASPControl::addControlErrorTemporary(QString message)
{
	if (_form && message.size())
		_form->addControlError(this, message, true);
}

void JASPControl::addControlErrorPermanent(QString message)
{
	if (_form && message.size())
		_form->addControlError(this, message, false, false, false);
}

void JASPControl::addControlWarning(QString message)
{
	if (_form && message.size())
		_form->addControlError(this, message, false, true);
}

void JASPControl::addControlWarningTemporary(QString message)
{
	if (_form && message.size())
		_form->addControlError(this, message, true, true);
}

void JASPControl::clearControlError()
{
	if (_form)
		_form->clearControlError(this);
}

QList<JASPControl*> JASPControl::getChildJASPControls(const QQuickItem * item, bool removeUnecessaryGroups)
{
	QList<JASPControl*> result;

	if (!item)
		return result;

	QList<QQuickItem*> childItems = item->childItems();

	for (QQuickItem* childItem : childItems)
	{
		JASPControl* childControl = qobject_cast<JASPControl*>(childItem);

		if (childControl)
		{
			if (removeUnecessaryGroups && childControl->controlType() == ControlType::GroupBox && childControl->title().isEmpty() && childControl->infoLabel().isEmpty() && childControl->info().isEmpty())
				// If a Group has no label, title or info, then it is used probably for layout purpose, but to structure the controls is sub elements.
				// Just skip it: this is necessary for generating properly the markdown help
				result.append(getChildJASPControls(childControl));
			else
				result.push_back(childControl);
		}
		else if (childItem->objectName() == "Section")
		{
			// The Expander button QML is not a JASPControl directly, it is composed by the button and a GridLayout wrapped up by a FocusScape
			// So take here the Button to have a real JASPControl.
			JASPControl* expanderButton = childItem->property("button").value<JASPControl*>();
			result.push_back(expanderButton);
		}
		else
			result.append(getChildJASPControls(childItem));
	}

	return result;
}

BoundControl *JASPControl::boundControl()
{
	if (isBound())	return dynamic_cast<BoundControl*>(this);
	return nullptr;
}

bool JASPControl::addDependency(JASPControl *item)
{
	if (_depends.count(item) > 0 || this == item || !item)
		return false;

	_depends.insert(item);
	return true;
}

void JASPControl::removeDependency(JASPControl *item)
{
	_depends.erase(item);
}

QString JASPControl::friendlyName() const
{
	return ControlTypeToFriendlyString(controlType());
}

void JASPControl::setParentDebug(bool parentDebug)
{
	if (_parentDebug != parentDebug)
	{
		_parentDebug = parentDebug;
		setParentDebugToChildren(_parentDebug || _debug);
		emit parentDebugChanged();
	}
}

void JASPControl::setFocusIndicator(QQuickItem *focusIndicator)
{
	if (focusIndicator != _focusIndicator)
	{
		if (focusIndicator)
		{
			QObject* border = focusIndicator->property("border").value<QObject*>();
			if (border)
			{
				_defaultBorderColor = border->property("color").value<QColor>();
				_defaultBorderWidth = border->property("width").toFloat();
			}
		}

		_focusIndicator = focusIndicator;

		emit focusIndicatorChanged();
	}
}

void JASPControl::_hightlightBorder()
{
	if (!_focusIndicator) return;

	QObject* border = _focusIndicator->property("border").value<QObject*>();
	if (!border) return;

	QColor	targetBorderColor = _defaultBorderColor,
			currentBorderColor = border->property("color").value<QColor>();

	JaspTheme* theme = JaspTheme::currentTheme();
	if (hasError())				targetBorderColor = theme->controlErrorTextColor();
	else if (hasWarning())		targetBorderColor = theme->controlWarningTextColor();
	else if (isDependency())	targetBorderColor = theme->dependencyBorderColor();
	else if (hasActiveFocus() && focusOnTab() && (!_innerControl || _innerControl->hasActiveFocus()))  targetBorderColor = theme->focusBorderColor();

	if (currentBorderColor != targetBorderColor)
		border->setProperty("color", targetBorderColor);

	float	targetBorderWidth = (targetBorderColor == _defaultBorderColor) ? _defaultBorderWidth : theme->jaspControlHighlightWidth(),
			currentBorderWidth = border->property("width").toFloat();

	if (!qFuzzyCompare(currentBorderWidth, targetBorderWidth))
	{
		_borderAnimation.stop();
		if (qFuzzyCompare(targetBorderWidth, _defaultBorderWidth))
			border->setProperty("width", targetBorderWidth); // No animation when coming back to normal.
		else
		{
			_borderAnimation.setTargetObject(border);
			_borderAnimation.setPropertyName("width");
			_borderAnimation.setDuration(800);
			_borderAnimation.setEasingCurve(QEasingCurve::OutElastic);
			_borderAnimation.setEndValue(targetBorderWidth);
			_borderAnimation.start();
		}
	}
}

void JASPControl::setDebug(bool debug)
{
	if (_debug != debug)
	{
		_debug = debug;
		setParentDebugToChildren(_parentDebug || _debug);
		emit debugChanged();
	}
}

void JASPControl::setParentDebugToChildren(bool debug)
{
	if (_childControlsArea)
		for (JASPControl* childControl : getChildJASPControls(_childControlsArea))
			childControl->setParentDebug(debug);
}

void JASPControl::focusInEvent(QFocusEvent *event)
{
	QQuickItem::focusInEvent(event);
	_focusReason = event->reason();
	_hasActiveFocus = true;
}

//Installed of innercontrol and non JASPControl children to capture focus reason
bool JASPControl::eventFilter(QObject *watched, QEvent *event)
{
	if (event->type() == QEvent::FocusIn)
	{
		QFocusEvent* focusEvent = static_cast<QFocusEvent*>(event);
		_focusReason = focusEvent->reason();
		_hasActiveFocus = true;
	}
	#ifdef __APPLE__
	if (event->type() == QEvent::MouseButtonPress)
	{
		QQuickItem* item = qobject_cast<QQuickItem*>(watched);
		if(item)
			item->forceActiveFocus(Qt::FocusReason::MouseFocusReason);
	}
	#endif
		return false;
}

void JASPControl::_checkControlName()
{
	checkOptionName(_name);
}

bool JASPControl::checkOptionName(const QString &name)
{
	// Do not check the option name if the control is not yet initialized: the isBound property is maybe not yet set
	// A RadioButton uses the name as value of a RadioButtonGroup option, so it's not an option self, so don't check it
	// (the RadioButtonGroup will take care that the Radio Button names are consistent.
	if (!form() || !initialized() || nameIsOptionValue()) return true;

	// Some controls without bound value, have a name (like Available Variables List). This name must also be checked
	if (!isBound() && name.isEmpty()) return true;

	// If a control is bound, it must have a name.
	if (isBound())
	{
		QString label = humanFriendlyLabel();

		if (name.isEmpty())
		{
			if (!label.isEmpty())	addControlError(tr("Control with label '%1' has no name").arg(label));
			else					addControlError(tr("A control has no name"));
		}
		return false;
	}

	if (_optionReservedNames.contains(name))
	{
		addControlError(tr("Option name '%1' is a reserved word").arg(name));
		return false;
	}

	JASPControl* anotherControl = form()->getControl(name);
	if (anotherControl && anotherControl != this)
	{
		addControlError(tr("2 controls have the same name: %1").arg(name));
		anotherControl->addControlError(tr("2 controls have the same name: %1").arg(name));
		return false;
	}

	if (form()->isFormulaName(name))
	{
		addControlError(tr("A control and a formula have the same name '%1'").arg(name));
		return false;
	}

	return true;
}

QString JASPControl::ControlTypeToFriendlyString(ControlType controlType)
{
	switch(controlType)
	{
	default:
	case ControlType::DefaultControl:				return tr("Option");				break;
	case ControlType::Expander:						return tr("Section");				break;
	case ControlType::CheckBox:						return tr("CheckBox");				break;
	case ControlType::Switch:						return tr("Switch");				break;
	case ControlType::TextField:					return tr("Entry Field");			break;
	case ControlType::RadioButton:					return tr("Radio Button");			break;
	case ControlType::RadioButtonGroup:				return tr("Radio Buttons");			break;
	case ControlType::VariablesListView:			return tr("Variables List");		break;
	case ControlType::ComboBox:						return tr("DropDown");				break;
	case ControlType::FactorLevelList:				return tr("Factor Level List");		break;
	case ControlType::InputListView:				return tr("Input ListView");		break;
	case ControlType::TableView:					return tr("TableView");				break;
	case ControlType::Slider:						return tr("Slider");				break;
	case ControlType::TextArea:						return tr("TextArea");				break;
	case ControlType::Button:						return tr("Button");				break;
	case ControlType::FactorsForm:					return tr("Factors Form");			break;
	case ControlType::ComponentsList:				return tr("List of Components");	break;
	case ControlType::GroupBox:						return tr("Group Box");				break;
	case ControlType::TabView:						return tr("Tab View");				break;
	case ControlType::VariablesForm:				return tr("Variables Form");		break;
	}
}

bool JASPControl::hasInfo() const
{
	if(!info().isEmpty()) return true;

	for (JASPControl* control : getChildJASPControls(_childControlsArea ? _childControlsArea : this))
		if (control->hasInfo()) return true;

	return false;
}

bool JASPControl::printLabelMD(QStringList& md, int depth) const
{
	QString label = (infoLabel().isEmpty() ? title() : infoLabel()).trimmed();
	if(label.isEmpty() && !infoAddControlType())
		return false;

	// Print the label as a header, in italic or in bold
	if (infoLabelIsHeader())			md << "<h" << QString::number(depth + 2) << ">";
	else if	(infoLabelItalic())			md << "*";
	else								md << "**";

	if (infoAddControlType())			md << (friendlyName() + (!label.isEmpty() ? " - " : ""));

	md << label;

	if (infoLabelIsHeader())			md << "</h" << QString::number(depth + 2) << ">\n";
	else
	{
		md << (infoLabelItalic() ? "*" : "**");
		if (!info().isEmpty() && !label.endsWith(":")) // Add ':' when necessary
			md << ":";
		md << " ";
	}

	return true;
}

QString JASPControl::helpMD(int depth) const
{
	if (!hasInfo()) return "";
		
	QStringList childMDs, markdown;

	for (JASPControl* childControl : getChildJASPControls(_childControlsArea ? _childControlsArea : this, true))
	{
		QString childMD = childControl->helpMD(depth + 1);
		if (!childMD.isEmpty())
			childMDs.push_back(childMD);
	}

	bool hasLabel = printLabelMD(markdown, depth);
	markdown << info() << "\n";

	if (infoLabelIsHeader() && !info().isEmpty())
		markdown << "\n"; // Special case when a header has no info (a Section without info eg).

	if (childMDs.length() == 1)
		markdown << QString{depth * 2, ' '} << childMDs[0];
	else
	{
		for (const QString& childMD : childMDs)
		{
			markdown << QString{depth * 2, ' '};
			if (hasLabel)
				markdown << "- "; // Add bullet list
			markdown << childMD;
			if (!hasLabel)
				markdown << "\n"; // If no bullet list is used, markdown needs an extra '\n' to display a new line
		}
	}

	return markdown.join("");;
}

void JASPControl::setChildControlsArea(QQuickItem * childControlsArea)
{
	_childControlsArea = childControlsArea;

	//If there is a child control we would like to be kept informed of it
	if(_childControlsArea)
		connect(_childControlsArea, &QQuickItem::childrenChanged, this, &JASPControl::reconnectWithYourChildren, Qt::UniqueConnection);

	//Just in case the children are there already:
	reconnectWithYourChildren();
}

void JASPControl::reconnectWithYourChildren()
{
	for (JASPControl* child : getChildJASPControls(_childControlsArea))
	{
		//Unique so that it doesn't matter how many times we connect
		connect(child, &JASPControl::helpMDChanged,		this, &JASPControl::helpMDChanged,		Qt::UniqueConnection);
		connect(child, &JASPControl::hasErrorChanged,	this, &JASPControl::hasErrorChanged,	Qt::UniqueConnection);
		connect(child, &JASPControl::hasWarningChanged, this, &JASPControl::hasWarningChanged,	Qt::UniqueConnection);
	}

	//Just in case:
	emit helpMDChanged();
	emit hasErrorChanged();
	emit hasWarningChanged();
}

void JASPControl::parentListViewKeyChanged(const QString &oldName, const QString &newName)
{
	if (oldName == _parentListViewKey)
		_parentListViewKey = newName;
}

void JASPControl::setName(const QString &name)
{
	if (name != _name && checkOptionName(name))
	{
		_name = name;
		emit nameChanged();
	}
}

bool JASPControl::hasError() const
{
	if(_controlType != ControlType::Expander)	return _hasError;
	else										return childHasError();
}

bool JASPControl::hasWarning() const
{
	if(_controlType != ControlType::Expander)	return _hasWarning;
	else										return childHasWarning();
}

bool JASPControl::childHasError() const
{
	for (JASPControl* child : getChildJASPControls(_childControlsArea))
		if(child->childHasError())
			return true;

	return _hasError;
}

bool JASPControl::childHasWarning() const
{
	for (JASPControl* child : getChildJASPControls(_childControlsArea))
		if(child->childHasWarning())
			return true;

	return _hasWarning;
}

// This method is just for the parentListView property that needs a JASPControl (JASPListControl is unknown in QML).
JASPControl *JASPControl::parentListViewEx() const
{
	return _parentListView;
}

bool JASPControl::hovered() const
{
	if (_mouseAreaObj)
		return _mouseAreaObj->property("containsMouse").toBool();
	else
		return false;
}



QString JASPControl::humanFriendlyLabel() const
{

	QString label = property("label").toString();

	if (label.isEmpty())
		label = property("title").toString();

	if (label.isEmpty())
		label = name();

	label = label.simplified();
	if (label.right(1) == ":")
		label = label.chopped(1);

	return label;

}

QVector<JASPControl::ParentKey> JASPControl::getParentKeys()
{
	QVector<JASPControl::ParentKey> parentKeys;
	JASPListControl* parentControl =  parentListView();
	QString parentKeyValue = parentListViewKey();

	while (parentControl)
	{
		parentKeys.prepend({parentControl->name().toStdString(), parentControl->optionKey().toStdString(), Term::readTerm(parentKeyValue).scomponents()});
		parentKeyValue = parentControl->parentListViewKey();
		parentControl = parentControl->parentListView();
	}

	return parentKeys;
}

void JASPControl::runRScript(const QString &script, bool whiteListedVersion)
{
	QString id = parentListView() ? (parentListView()->name() + "." + parentListViewKey() + "." + name()) : name();

	form()->runRScript(script, id, whiteListedVersion);
}

void JASPControl::runFilter(const QString & name)
{
	form()->runFilter(name);
}

void JASPControl::rScriptDoneHandler(const QString &)
{
	throw std::runtime_error("runRScript done but handler not implemented!\nImplement an override for rScriptDoneHandler\n");
}

void JASPControl::filterDoneHandler(const QString &name, const QString & error)
{
	//throw std::runtime_error("runFilter done but handler not implemented!\nImplement an override for filterDoneHandler\n");
	//No need to be annoying about it, each control that cares about a particular filter can just check it and the default does nothing.
}

void JASPControl::_setFocus()
{
	if (!hasActiveFocus())
		setFocus(false);
}

void JASPControl::_notifyFormOfActiveFocus()
{
	if (!hasActiveFocus())
		_hasActiveFocus = false;

	//All JASP controls are focusscopes when they receive focus due to a child being clicked the reason is 8 for some reason (undocumented as of 3-1-2023)
	//Objects like jasp groupboxes could unrightfully be marked as the active jasp control when a child jasp control gets focus
	//For this reason we check if the focus reason is the result of user input or the focus scope system.
	if (_form && _focusReason >= Qt::MouseFocusReason && _focusReason <= Qt::OtherFocusReason)
		_form->setActiveJASPControl(this, hasActiveFocus());
}

void JASPControl::_addExplicitDependency(const QVariant& depends)
{
	if (!depends.isValid() || depends.isNull()) return;

	JASPControl* control = depends.value<JASPControl*>();
	if (control)
		_depends.insert(control);
	else if (depends.canConvert<QString>())
		_depends.insert(form()->getControl(depends.toString()));
	else if (depends.canConvert<QVariantList>())
	{
		QVariantList varDeps = depends.toList();
		for (const QVariant& varDep : varDeps)
			_addExplicitDependency(varDep);
	}
}

void JASPControl::addExplicitDependency()
{
	_addExplicitDependency(_explicitDepends);
}

bool JASPControl::dependingControlsAreInitialized()
{
	bool dependenciesAreInitialized = true;

	for (JASPControl* c : _depends)
	{
		if (!c->initialized())
			dependenciesAreInitialized = false;
	}
	return dependenciesAreInitialized;
}

void JASPControl::setInitialized(const Json::Value &value)
{
	if (dependingControlsAreInitialized())
		_setInitialized(value);
	else
	{
		for (JASPControl* c : _depends)
			if (!c->initialized())
				connect(c, &JASPControl::initializedChanged, this, [this, value]() { if (dependingControlsAreInitialized()) _setInitialized(value); });
	}
}

void JASPControl::_setInitialized(const Json::Value &value)
{
	BoundControl* bControl = boundControl();
	if (bControl)
	{
		if (!_initialized) // If it was already initialized, don't reset its default value: this can be the case when R syntax is re-run
			bControl->setDefaultBoundValue(bControl->createJson());
		bControl->bindTo(value == Json::nullValue ? bControl->createJson() : value);
	}

	_initialized = true;
	_initializedWithValue = (value != Json::nullValue);
	emit initializedChanged();
}		
