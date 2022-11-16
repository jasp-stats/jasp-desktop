#include "jasptheme.h"
#include "log.h"
#include "utilities/qutils.h"
#include <QFontDatabase>

JaspTheme			*	JaspTheme::_currentTheme	= nullptr;
QFontMetricsF			JaspTheme::_fontMetrics		= QFontMetricsF(QFont());

std::map<QString, JaspTheme *> JaspTheme::_themes;

JaspTheme::JaspTheme(QQuickItem * parent) : QQuickItem(parent)
{
	connectSizeDistancesToUiScaleChanged();

	if(_currentTheme == nullptr)
		setCurrentTheme(this);

	//_fontCode.setStyleHint(QFont::Monospace); // Cannot be set in QML https://bugreports.qt.io/browse/QTBUG-38931

	updateFontMetrics();

	connect(this,						&JaspTheme::uiScaleChanged,					this,						&JaspTheme::updateFontMetrics			);
}

JaspTheme::~JaspTheme()
{
	if(_currentTheme == this)
		setCurrentTheme(nullptr);
}

#define CONNECT_UISCALE(toThis) connect(this, &JaspTheme::uiScaleChanged, this, &JaspTheme::toThis)

void JaspTheme::connectSizeDistancesToUiScaleChanged()
{
	CONNECT_UISCALE(borderRadiusChanged);
	CONNECT_UISCALE(shadowRadiusChanged);
	CONNECT_UISCALE(itemPaddingChanged);
	CONNECT_UISCALE(jaspControlPaddingChanged);
	CONNECT_UISCALE(ribbonButtonPaddingChanged);
	CONNECT_UISCALE(groupContentPaddingChanged);
	CONNECT_UISCALE(rowSpacingChanged);
	CONNECT_UISCALE(rowGridSpacingChanged);
	CONNECT_UISCALE(rowGroupSpacingChanged);
	CONNECT_UISCALE(columnGridSpacingChanged);
	CONNECT_UISCALE(columnGroupSpacingChanged);
	CONNECT_UISCALE(indentationLengthChanged);
	CONNECT_UISCALE(labelSpacingChanged);
	CONNECT_UISCALE(menuSpacingChanged);
	CONNECT_UISCALE(menuPaddingChanged);
	CONNECT_UISCALE(generalAnchorMarginChanged);
	CONNECT_UISCALE(generalMenuMarginChanged);
	CONNECT_UISCALE(titleBottomMarginChanged);
	CONNECT_UISCALE(contentMarginChanged);
	CONNECT_UISCALE(subOptionOffsetChanged);
	CONNECT_UISCALE(minPanelWidthChanged);
	CONNECT_UISCALE(resultWidthChanged);
	CONNECT_UISCALE(formWidthChanged);
	CONNECT_UISCALE(iconSizeChanged);
	CONNECT_UISCALE(formMarginChanged);
	CONNECT_UISCALE(formExpanderHeaderHeightChanged);
	CONNECT_UISCALE(sliderWidthChanged);
	CONNECT_UISCALE(sliderLengthChanged);
	CONNECT_UISCALE(switchHeightChanged);
	CONNECT_UISCALE(spinBoxHeightChanged);
	CONNECT_UISCALE(spinBoxWidthChanged);
	CONNECT_UISCALE(comboBoxHeightChanged);
	CONNECT_UISCALE(textFieldWidthChanged);
	CONNECT_UISCALE(textFieldHeightChanged);
	CONNECT_UISCALE(numericFieldWidthChanged);
	CONNECT_UISCALE(splitHandleWidthChanged);
	CONNECT_UISCALE(subMenuIconHeightChanged);
	CONNECT_UISCALE(ribbonButtonHeightChanged);
	CONNECT_UISCALE(variablesListTitleChanged);
	CONNECT_UISCALE(sliderHandleDiameterChanged);
	CONNECT_UISCALE(defaultTextAreaHeightChanged);
	CONNECT_UISCALE(jaspControlHighlightWidthChanged);
	CONNECT_UISCALE(defaultVariablesFormHeightChanged);
	CONNECT_UISCALE(defaultSingleItemListHeightChanged);
	CONNECT_UISCALE(defaultRectangularButtonHeightChanged);
	CONNECT_UISCALE(smallDefaultVariablesFormHeightChanged);
	CONNECT_UISCALE(messageBoxButtonHeightChanged);
	CONNECT_UISCALE(scrollbarBoxWidthBigChanged);
	CONNECT_UISCALE(scrollbarBoxWidthChanged);
	CONNECT_UISCALE(menuItemHeightChanged);
	CONNECT_UISCALE(menuGroupTitleHeightChanged);
	CONNECT_UISCALE(menuHeaderHeightChanged);
}

void JaspTheme::setCurrentTheme(JaspTheme * theme)
{
	if(theme == _currentTheme)
		return;

	_currentTheme = theme;

	if(_currentTheme)
	{
		_currentTheme->updateFontMetrics();
		emit _currentTheme->currentThemeReady(theme);
	}
}

void JaspTheme::setCurrentThemeFromName(QString name)
{
	if(name == "")
		return;

	//Log::log() << "void JaspTheme::setCurrentThemeFromName( "<< name << " )" << std::endl;

	if(_themes.count(name) == 0)
	{
		Log::log() << "Could not find theme " << name << std::endl;
		return;
	}

	setCurrentTheme(_themes[name]);
}

void JaspTheme::setRibbonScaleHovered(float ribbonScaleHovered)
{
	
	if (qFuzzyCompare(_ribbonScaleHovered, ribbonScaleHovered))
		return;

	_ribbonScaleHovered = ribbonScaleHovered;
	emit ribbonScaleHoveredChanged(_ribbonScaleHovered);
}

void JaspTheme::setWhite(QColor white)
{
	if (_white == white)
		return;

	_white = white;
	emit whiteChanged(_white);
}

void JaspTheme::setWhiteBroken(QColor whiteBroken)
{
	if (_whiteBroken == whiteBroken)
		return;

	_whiteBroken = whiteBroken;
	emit whiteBrokenChanged(_whiteBroken);
}

void JaspTheme::setBlack(QColor black)
{
	if (_black == black)
		return;

	_black = black;
	emit blackChanged(_black);
}

void JaspTheme::setGray(QColor gray)
{
	if (_gray == gray)
		return;

	_gray = gray;
	emit grayChanged(_gray);
}

void JaspTheme::setGrayDarker(QColor grayDarker)
{
	if (_grayDarker == grayDarker)
		return;

	_grayDarker = grayDarker;
	emit grayDarkerChanged(_grayDarker);
}

void JaspTheme::setGrayLighter(QColor grayLighter)
{
	if (_grayLighter == grayLighter)
		return;

	_grayLighter = grayLighter;
	emit grayLighterChanged(_grayLighter);
}

void JaspTheme::setGrayMuchLighter(QColor grayMuchLighter)
{
	if (_grayMuchLighter == grayMuchLighter)
		return;

	_grayMuchLighter = grayMuchLighter;
	emit grayMuchLighterChanged(_grayMuchLighter);
}

void JaspTheme::setGrayVeryMuchLighter(QColor grayVeryMuchLighter)
{
	if (_grayVeryMuchLighter == grayVeryMuchLighter)
		return;

	_grayVeryMuchLighter = grayVeryMuchLighter;
	emit grayVeryMuchLighterChanged(_grayVeryMuchLighter);
}

void JaspTheme::setBlue(QColor blue)
{
	if (_blue == blue)
		return;

	_blue = blue;
	emit blueChanged(_blue);
}

void JaspTheme::setBlueDarker(QColor blueDarker)
{
	if (_blueDarker == blueDarker)
		return;

	_blueDarker = blueDarker;
	emit blueDarkerChanged(_blueDarker);
}

void JaspTheme::setBlueLighter(QColor blueLighter)
{
	if (_blueLighter == blueLighter)
		return;

	_blueLighter = blueLighter;
	emit blueLighterChanged(_blueLighter);
}

void JaspTheme::setBlueMuchLighter(QColor blueMuchLighter)
{
	if (_blueMuchLighter == blueMuchLighter)
		return;

	_blueMuchLighter = blueMuchLighter;
	emit blueMuchLighterChanged(_blueMuchLighter);
}

void JaspTheme::setRed(QColor red)
{
	if (_red == red)
		return;

	_red = red;
	emit redChanged(_red);
}

void JaspTheme::setRedDarker(QColor redDarker)
{
	if (_redDarker == redDarker)
		return;

	_redDarker = redDarker;
	emit redDarkerChanged(_redDarker);
}

void JaspTheme::setGreen(QColor green)
{
	if (_green == green)
		return;

	_green = green;
	emit greenChanged(_green);
}

void JaspTheme::setYellowLight(QColor yellowLight)
{
	if (_yellowLight == yellowLight)
		return;

	_yellowLight = yellowLight;
	emit yellowLightChanged(_yellowLight);
}

void JaspTheme::setRose(QColor rose)
{
	if (_rose == rose)
		return;

	_rose = rose;
	emit roseChanged(_rose);
}

void JaspTheme::setRoseLight(QColor roseLight)
{
	if (_roseLight == roseLight)
		return;

	_roseLight = roseLight;
	emit roseLightChanged(_roseLight);
}

void JaspTheme::setCyan(QColor cyan)
{
	if (_cyan == cyan)
		return;

	_cyan = cyan;
	emit cyanChanged(_cyan);
}

void JaspTheme::setShadow(QColor shadow)
{
	if (_shadow == shadow)
		return;

	_shadow = shadow;
	emit shadowChanged(_shadow);
}


void JaspTheme::setJaspBlue(QColor jaspBlue)
{
	if (_jaspBlue == jaspBlue)
		return;

	_jaspBlue = jaspBlue;
	emit jaspBlueChanged(_jaspBlue);
}

void JaspTheme::setJaspGreen(QColor jaspGreen)
{
	if (_jaspGreen == jaspGreen)
		return;

	_jaspGreen = jaspGreen;
	emit jaspGreenChanged(_jaspGreen);
}

void JaspTheme::setTextEnabled(QColor textEnabled)
{
	if (_textEnabled == textEnabled)
		return;

	_textEnabled = textEnabled;
	emit textEnabledChanged(_textEnabled);
}

void JaspTheme::setTextDisabled(QColor textDisabled)
{
	if (_textDisabled == textDisabled)
		return;

	_textDisabled = textDisabled;
	emit textDisabledChanged(_textDisabled);
}

void JaspTheme::setUiBackground(QColor uiBackground)
{
	if (_uiBackground == uiBackground)
		return;

	_uiBackground = uiBackground;
	emit uiBackgroundChanged(_uiBackground);
}

void JaspTheme::setUiBorder(QColor uiBorder)
{
	if (_uiBorder == uiBorder)
		return;

	_uiBorder = uiBorder;
	emit uiBorderChanged(_uiBorder);
}

void JaspTheme::setFileMenuColorBackground(QColor fileMenuColorBackground)
{
	if (_fileMenuColorBackground == fileMenuColorBackground)
		return;

	_fileMenuColorBackground = fileMenuColorBackground;
	emit fileMenuColorBackgroundChanged(_fileMenuColorBackground);
}

void JaspTheme::setFileMenuLightBorder(QColor fileMenuLightBorder)
{
	if (_fileMenuLightBorder == fileMenuLightBorder)
		return;

	_fileMenuLightBorder = fileMenuLightBorder;
	emit fileMenuLightBorderChanged(_fileMenuLightBorder);
}

void JaspTheme::setButtonColor(QColor buttonColor)
{
	if (_buttonColor == buttonColor)
		return;

	_buttonColor = buttonColor;
	emit buttonColorChanged(_buttonColor);
}

void JaspTheme::setButtonColorHovered(QColor buttonColorHovered)
{
	if (_buttonColorHovered == buttonColorHovered)
		return;

	_buttonColorHovered = buttonColorHovered;
	emit buttonColorHoveredChanged(_buttonColorHovered);
}

void JaspTheme::setButtonColorPressed(QColor buttonColorPressed)
{
	if (_buttonColorPressed == buttonColorPressed)
		return;

	_buttonColorPressed = buttonColorPressed;
	emit buttonColorPressedChanged(_buttonColorPressed);
}

void JaspTheme::setButtonBorderColor(QColor buttonBorderColor)
{
	if (_buttonBorderColor == buttonBorderColor)
		return;

	_buttonBorderColor = buttonBorderColor;
	emit buttonBorderColorChanged(_buttonBorderColor);
}

void JaspTheme::setButtonBorderColorHovered(QColor buttonBorderColorHovered)
{
	if (_buttonBorderColorHovered == buttonBorderColorHovered)
		return;

	_buttonBorderColorHovered = buttonBorderColorHovered;
	emit buttonBorderColorHoveredChanged(_buttonBorderColorHovered);
}

void JaspTheme::setItemHighlight(QColor itemHighlight)
{
	if (_itemHighlight == itemHighlight)
		return;

	_itemHighlight = itemHighlight;
	emit itemHighlightChanged(_itemHighlight);
}

void JaspTheme::setItemHoverColor(QColor itemHoverColor)
{
	if (_itemHoverColor == itemHoverColor)
		return;

	_itemHoverColor = itemHoverColor;
	emit itemHoverColorChanged(_itemHoverColor);
}

void JaspTheme::setItemSelectedColor(QColor itemSelectedColor)
{
	if (_itemSelectedColor == itemSelectedColor)
		return;

	_itemSelectedColor = itemSelectedColor;
	emit itemSelectedColorChanged(_itemSelectedColor);
}

void JaspTheme::setItemSelectedNoFocusColor(QColor itemSelectedNoFocusColor)
{
	if (_itemSelectedNoFocusColor == itemSelectedNoFocusColor)
		return;

	_itemSelectedNoFocusColor = itemSelectedNoFocusColor;
	emit itemSelectedNoFocusColorChanged(_itemSelectedNoFocusColor);
}

void JaspTheme::setBorderColor(QColor borderColor)
{
	if (_borderColor == borderColor)
		return;

	_borderColor = borderColor;
	emit borderColorChanged(_borderColor);
}

void JaspTheme::setFocusBorderColor(QColor focusBorderColor)
{
	if (_focusBorderColor == focusBorderColor)
		return;

	_focusBorderColor = focusBorderColor;
	emit focusBorderColorChanged(_focusBorderColor);
}

void JaspTheme::setDependencyBorderColor(QColor dependencyBorderColor)
{
	if (_dependencyBorderColor == dependencyBorderColor)
		return;

	_dependencyBorderColor = dependencyBorderColor;
	emit dependencyBorderColorChanged(_dependencyBorderColor);
}

void JaspTheme::setDependencySelectedColor(QColor dependencySelectedColor)
{
	if (_dependencySelectedColor == dependencySelectedColor)
		return;

	_dependencySelectedColor = dependencySelectedColor;
	emit dependencySelectedColorChanged(_dependencySelectedColor);
}

void JaspTheme::setContainsDragBorderColor(QColor containsDragBorderColor)
{
	if (_containsDragBorderColor == containsDragBorderColor)
		return;

	_containsDragBorderColor = containsDragBorderColor;
	emit containsDragBorderColorChanged(_containsDragBorderColor);
}

void JaspTheme::setAnalysisBackgroundColor(QColor analysisBackgroundColor)
{
	if (_analysisBackgroundColor == analysisBackgroundColor)
		return;

	_analysisBackgroundColor = analysisBackgroundColor;
	emit analysisBackgroundColorChanged(_analysisBackgroundColor);
}

void JaspTheme::setControlBackgroundColor(QColor controlBackgroundColor)
{
	if (_controlBackgroundColor == controlBackgroundColor)
		return;

	_controlBackgroundColor = controlBackgroundColor;
	emit controlBackgroundColorChanged(_controlBackgroundColor);
}

void JaspTheme::setControlDisabledBackgroundColor(QColor controlDisabledBackgroundColor)
{
	if (_controlDisabledBackgroundColor == controlDisabledBackgroundColor)
		return;

	_controlDisabledBackgroundColor = controlDisabledBackgroundColor;
	emit controlDisabledBackgroundColorChanged(_controlDisabledBackgroundColor);
}

void JaspTheme::setRowEvenColor(QColor rowEvenColor)
{
	if (_rowEvenColor == rowEvenColor)
		return;

	_rowEvenColor = rowEvenColor;
	emit rowEvenColorChanged(_rowEvenColor);
}

void JaspTheme::setRowOnevenColor(QColor rowOnevenColor)
{
	if (_rowOnevenColor == rowOnevenColor)
		return;

	_rowOnevenColor = rowOnevenColor;
	emit rowOnevenColorChanged(_rowOnevenColor);
}

void JaspTheme::setControlErrorBackgroundColor(QColor controlErrorBackgroundColor)
{
	if (_controlErrorBackgroundColor == controlErrorBackgroundColor)
		return;

	_controlErrorBackgroundColor = controlErrorBackgroundColor;
	emit controlErrorBackgroundColorChanged(_controlErrorBackgroundColor);
}

void JaspTheme::setControlErrorTextColor(QColor controlErrorTextColor)
{
	if (_controlErrorTextColor == controlErrorTextColor)
		return;

	_controlErrorTextColor = controlErrorTextColor;
	emit controlErrorTextColorChanged(_controlErrorTextColor);
}

void JaspTheme::setControlWarningBackgroundColor(QColor controlWarningBackgroundColor)
{
	if (_controlWarningBackgroundColor == controlWarningBackgroundColor)
		return;

	_controlWarningBackgroundColor = controlWarningBackgroundColor;
	emit controlWarningBackgroundColorChanged(_controlWarningBackgroundColor);
}

void JaspTheme::setControlWarningTextColor(QColor controlWarningTextColor)
{
	if (_controlWarningTextColor == controlWarningTextColor)
		return;

	_controlWarningTextColor = controlWarningTextColor;
	emit controlWarningTextColorChanged(_controlWarningTextColor);
}

void JaspTheme::setButtonBackgroundColor(QColor buttonBackgroundColor)
{
	if (_buttonBackgroundColor == buttonBackgroundColor)
		return;

	_buttonBackgroundColor = buttonBackgroundColor;
	emit buttonBackgroundColorChanged(_buttonBackgroundColor);
}

void JaspTheme::setTooltipBackgroundColor(QColor tooltipBackgroundColor)
{
	if (_tooltipBackgroundColor == tooltipBackgroundColor)
		return;

	_tooltipBackgroundColor = tooltipBackgroundColor;
	emit tooltipBackgroundColorChanged(_tooltipBackgroundColor);
}

void JaspTheme::setDebugBackgroundColor(QColor debugBackgroundColor)
{
	if (_debugBackgroundColor == debugBackgroundColor)
		return;

	_debugBackgroundColor = debugBackgroundColor;
	emit debugBackgroundColorChanged(_debugBackgroundColor);
}

void JaspTheme::setErrorMessagesBackgroundColor(QColor errorMessagesBackgroundColor)
{
	if (_errorMessagesBackgroundColor == errorMessagesBackgroundColor)
		return;

	_errorMessagesBackgroundColor = errorMessagesBackgroundColor;
	emit errorMessagesBackgroundColorChanged(_errorMessagesBackgroundColor);
}

void JaspTheme::setSliderPartOn(QColor sliderPartOn)
{
	if (_sliderPartOn == sliderPartOn)
		return;

	_sliderPartOn = sliderPartOn;
	emit sliderPartOnChanged(_sliderPartOn);
}

void JaspTheme::setSliderPartOff(QColor sliderPartOff)
{
	if (_sliderPartOff == sliderPartOff)
		return;

	_sliderPartOff = sliderPartOff;
	emit sliderPartOffChanged(_sliderPartOff);
}

void JaspTheme::setBorderRadius(theme_distanceType borderRadius)
{
	if (_borderRadius == borderRadius)
		return;

	_borderRadius = borderRadius;
	emit borderRadiusChanged();
}

void JaspTheme::setShadowRadius(theme_distanceType shadowRadius)
{
	if (_shadowRadius == shadowRadius)
		return;

	_shadowRadius = shadowRadius;
	emit shadowRadiusChanged();
}

void JaspTheme::setItemPadding(theme_distanceType itemPadding)
{
	if (_itemPadding == itemPadding)
		return;

	_itemPadding = itemPadding;
	emit itemPaddingChanged();
}

void JaspTheme::setJaspControlPadding(theme_distanceType jaspControlPadding)
{
	if (_jaspControlPadding == jaspControlPadding)
		return;

	_jaspControlPadding = jaspControlPadding;
	emit jaspControlPaddingChanged();
}

void JaspTheme::setRibbonButtonPadding(theme_distanceType ribbonButtonPadding)
{
	if (_ribbonButtonPadding == ribbonButtonPadding)
		return;

	_ribbonButtonPadding = ribbonButtonPadding;
	emit ribbonButtonPaddingChanged();
}

void JaspTheme::setGroupContentPadding(theme_distanceType groupContentPadding)
{
	if (_groupContentPadding == groupContentPadding)
		return;

	_groupContentPadding = groupContentPadding;
	emit groupContentPaddingChanged();
}

void JaspTheme::setRowSpacing(theme_distanceType rowSpacing)
{
	if (_rowSpacing == rowSpacing)
		return;

	_rowSpacing = rowSpacing;
	emit rowSpacingChanged();
}

void JaspTheme::setRowGridSpacing(theme_distanceType rowGridSpacing)
{
	if (_rowGridSpacing == rowGridSpacing)
		return;

	_rowGridSpacing = rowGridSpacing;
	emit rowGridSpacingChanged();
}

void JaspTheme::setRowGroupSpacing(theme_distanceType rowGroupSpacing)
{
	if (_rowGroupSpacing == rowGroupSpacing)
		return;

	_rowGroupSpacing = rowGroupSpacing;
	emit rowGroupSpacingChanged();
}

void JaspTheme::setColumnGridSpacing(theme_distanceType columnGridSpacing)
{
	if (_columnGridSpacing == columnGridSpacing)
		return;

	_columnGridSpacing = columnGridSpacing;
	emit columnGridSpacingChanged();
}

void JaspTheme::setColumnGroupSpacing(theme_distanceType columnGroupSpacing)
{
	if (_columnGroupSpacing == columnGroupSpacing)
		return;

	_columnGroupSpacing = columnGroupSpacing;
	emit columnGroupSpacingChanged();
}

void JaspTheme::setIndentationLength(theme_distanceType indentationLength)
{
	if (_indentationLength == indentationLength)
		return;

	_indentationLength = indentationLength;
	emit indentationLengthChanged();
}

void JaspTheme::setLabelSpacing(theme_distanceType labelSpacing)
{
	if (_labelSpacing == labelSpacing)
		return;

	_labelSpacing = labelSpacing;
	emit labelSpacingChanged();
}

void JaspTheme::setMenuSpacing(theme_distanceType menuSpacing)
{
	if (_menuSpacing == menuSpacing)
		return;

	_menuSpacing = menuSpacing;
	emit menuSpacingChanged();
}

void JaspTheme::setMenuPadding(theme_distanceType menuPadding)
{
	if (_menuPadding == menuPadding)
		return;

	_menuPadding = menuPadding;
	emit menuPaddingChanged();
}

void JaspTheme::setGeneralAnchorMargin(theme_distanceType generalAnchorMargin)
{
	if (_generalAnchorMargin == generalAnchorMargin)
		return;

	_generalAnchorMargin = generalAnchorMargin;
	emit generalAnchorMarginChanged();
}

void JaspTheme::setGeneralMenuMargin(theme_distanceType generalMenuMargin)
{
	if (_generalMenuMargin == generalMenuMargin)
		return;

	_generalMenuMargin = generalMenuMargin;
	emit generalMenuMarginChanged();
}

void JaspTheme::setTitleBottomMargin(theme_distanceType titleBottomMargin)
{
	if (_titleBottomMargin == titleBottomMargin)
		return;

	_titleBottomMargin = titleBottomMargin;
	emit titleBottomMarginChanged();
}

void JaspTheme::setContentMargin(theme_distanceType contentMargin)
{
	if (_contentMargin == contentMargin)
		return;

	_contentMargin = contentMargin;
	emit contentMarginChanged();
}

void JaspTheme::setSubOptionOffset(theme_distanceType subOptionOffset)
{
	if (_subOptionOffset == subOptionOffset)
		return;

	_subOptionOffset = subOptionOffset;
	emit subOptionOffsetChanged();
}

void JaspTheme::setMinPanelWidth(theme_sizeType minPanelWidth)
{
	if (_minPanelWidth == minPanelWidth)
		return;

	_minPanelWidth = minPanelWidth;
	emit minPanelWidthChanged();
}

void JaspTheme::setResultWidth(theme_sizeType resultWidth)
{
	if (_resultWidth == resultWidth)
		return;

	_resultWidth = resultWidth;
	emit resultWidthChanged();
}

void JaspTheme::setFormWidth(theme_sizeType formWidth)
{
	if (_formWidth == formWidth)
		return;

	_formWidth = formWidth;
	emit formWidthChanged();
}

void JaspTheme::setIconSize(theme_sizeType iconSize)
{
	if (_iconSize == iconSize)
		return;

	_iconSize = iconSize;
	emit iconSizeChanged();
}

void JaspTheme::setFormMargin(theme_sizeType formMargin)
{
	if (_formMargin == formMargin)
		return;

	_formMargin = formMargin;
	emit formMarginChanged();
}

void JaspTheme::setFormExpanderHeaderHeight(theme_sizeType formExpanderHeaderHeight)
{
	if (_formExpanderHeaderHeight == formExpanderHeaderHeight)
		return;

	_formExpanderHeaderHeight = formExpanderHeaderHeight;
	emit formExpanderHeaderHeightChanged();
}

void JaspTheme::setSliderWidth(theme_sizeType sliderWidth)
{
	if (_sliderWidth == sliderWidth)
		return;

	_sliderWidth = sliderWidth;
	emit sliderWidthChanged();
}

void JaspTheme::setSliderLength(theme_sizeType sliderLength)
{
	if (_sliderLength == sliderLength)
		return;

	_sliderLength = sliderLength;
	emit sliderLengthChanged();
}

void JaspTheme::setSwitchHeight(theme_sizeType switchHeight)
{
	if (_switchHeight == switchHeight)
		return;

	_switchHeight = switchHeight;
	emit switchHeightChanged();
}

void JaspTheme::setSpinBoxHeight(theme_sizeType spinBoxHeight)
{
	if (_spinBoxHeight == spinBoxHeight)
		return;

	_spinBoxHeight = spinBoxHeight;
	emit spinBoxHeightChanged();
}

void JaspTheme::setSpinBoxWidth(theme_sizeType spinBoxWidth)
{
	if (_spinBoxWidth == spinBoxWidth)
		return;

	_spinBoxWidth = spinBoxWidth;
	emit spinBoxWidthChanged();
}

void JaspTheme::setComboBoxHeight(theme_sizeType comboBoxHeight)
{
	if (_comboBoxHeight == comboBoxHeight)
		return;

	_comboBoxHeight = comboBoxHeight;
	emit comboBoxHeightChanged();
}

void JaspTheme::setTextFieldWidth(theme_sizeType textFieldWidth)
{
	if (_textFieldWidth == textFieldWidth)
		return;

	_textFieldWidth = textFieldWidth;
	emit textFieldWidthChanged();
}

void JaspTheme::setTextFieldHeight(theme_sizeType textFieldHeight)
{
	if (_textFieldHeight == textFieldHeight)
		return;

	_textFieldHeight = textFieldHeight;
	emit textFieldHeightChanged();
}

void JaspTheme::setNumericFieldWidth(theme_sizeType numericFieldWidth)
{
	if (_numericFieldWidth == numericFieldWidth)
		return;

	_numericFieldWidth = numericFieldWidth;
	emit numericFieldWidthChanged();
}

void JaspTheme::setSplitHandleWidth(theme_sizeType splitHandleWidth)
{
	if (_splitHandleWidth == splitHandleWidth)
		return;

	_splitHandleWidth = splitHandleWidth;
	emit splitHandleWidthChanged();
}

void JaspTheme::setSubMenuIconHeight(theme_sizeType subMenuIconHeight)
{
	if (_subMenuIconHeight == subMenuIconHeight)
		return;

	_subMenuIconHeight = subMenuIconHeight;
	emit subMenuIconHeightChanged();
}

void JaspTheme::setRibbonButtonHeight(theme_sizeType ribbonButtonHeight)
{
	if (_ribbonButtonHeight == ribbonButtonHeight)
		return;

	_ribbonButtonHeight = ribbonButtonHeight;
	emit ribbonButtonHeightChanged();
}

void JaspTheme::setVariablesListTitle(theme_sizeType variablesListTitle)
{
	if (_variablesListTitle == variablesListTitle)
		return;

	_variablesListTitle = variablesListTitle;
	emit variablesListTitleChanged();
}

void JaspTheme::setSliderHandleDiameter(theme_sizeType sliderHandleDiameter)
{
	if (_sliderHandleDiameter == sliderHandleDiameter)
		return;

	_sliderHandleDiameter = sliderHandleDiameter;
	emit sliderHandleDiameterChanged();
}

void JaspTheme::setDefaultTextAreaHeight(theme_sizeType defaultTextAreaHeight)
{
	if (_defaultTextAreaHeight == defaultTextAreaHeight)
		return;

	_defaultTextAreaHeight = defaultTextAreaHeight;
	emit defaultTextAreaHeightChanged();
}

void JaspTheme::setJaspControlHighlightWidth(theme_sizeType jaspControlHighlightWidth)
{
	if (_jaspControlHighlightWidth == jaspControlHighlightWidth)
		return;

	_jaspControlHighlightWidth = jaspControlHighlightWidth;
	emit jaspControlHighlightWidthChanged();
}

void JaspTheme::setDefaultVariablesFormHeight(theme_sizeType defaultVariablesFormHeight)
{
	if (_defaultVariablesFormHeight == defaultVariablesFormHeight)
		return;

	_defaultVariablesFormHeight = defaultVariablesFormHeight;
	emit defaultVariablesFormHeightChanged();
}

void JaspTheme::setDefaultSingleItemListHeight(theme_sizeType defaultSingleItemListHeight)
{
	if (_defaultSingleItemListHeight == defaultSingleItemListHeight)
		return;

	_defaultSingleItemListHeight = defaultSingleItemListHeight;
	emit defaultSingleItemListHeightChanged();
}

void JaspTheme::setDefaultRectangularButtonHeight(theme_sizeType defaultRectangularButtonHeight)
{
	if (_defaultRectangularButtonHeight == defaultRectangularButtonHeight)
		return;

	_defaultRectangularButtonHeight = defaultRectangularButtonHeight;
	emit defaultRectangularButtonHeightChanged();
}

void JaspTheme::setSmallDefaultVariablesFormHeight(theme_sizeType smallDefaultVariablesFormHeight)
{
	if (_smallDefaultVariablesFormHeight == smallDefaultVariablesFormHeight)
		return;

	_smallDefaultVariablesFormHeight = smallDefaultVariablesFormHeight;
	emit smallDefaultVariablesFormHeightChanged();
}

void JaspTheme::setMessageBoxButtonHeight(theme_sizeType messageBoxButtonHeight)
{
	if (_messageBoxButtonHeight == messageBoxButtonHeight)
		return;

	_messageBoxButtonHeight = messageBoxButtonHeight;
	emit messageBoxButtonHeightChanged();
}

void JaspTheme::setScrollbarBoxWidthBig(theme_sizeType scrollbarBoxWidthBig)
{
	if (_scrollbarBoxWidthBig == scrollbarBoxWidthBig)
		return;

	_scrollbarBoxWidthBig = scrollbarBoxWidthBig;
	emit scrollbarBoxWidthBigChanged();
}

void JaspTheme::setScrollbarBoxWidth(theme_sizeType scrollbarBoxWidth)
{
	if (_scrollbarBoxWidth == scrollbarBoxWidth)
		return;

	_scrollbarBoxWidth = scrollbarBoxWidth;
	emit scrollbarBoxWidthChanged();
}

void JaspTheme::setMenuItemHeight(theme_sizeType menuItemHeight)
{
	if (_menuItemHeight == menuItemHeight)
		return;

	_menuItemHeight = menuItemHeight;
	emit menuItemHeightChanged();
}

void JaspTheme::setMenuGroupTitleHeight(theme_sizeType menuGroupTitleHeight)
{
	if (_menuGroupTitleHeight == menuGroupTitleHeight)
		return;

	_menuGroupTitleHeight = menuGroupTitleHeight;
	emit menuGroupTitleHeightChanged();
}

void JaspTheme::setMenuHeaderHeight(theme_sizeType menuHeaderHeight)
{
	if (_menuHeaderHeight == menuHeaderHeight)
		return;

	_menuHeaderHeight = menuHeaderHeight;
	emit menuHeaderHeightChanged();
}

void JaspTheme::setHoverTime(theme_timeType hoverTime)
{
	if (_hoverTime == hoverTime)
		return;

	_hoverTime = hoverTime;
	emit hoverTimeChanged(_hoverTime);
}

void JaspTheme::setFileMenuSlideDuration(theme_timeType fileMenuSlideDuration)
{
	if (_fileMenuSlideDuration == fileMenuSlideDuration)
		return;

	_fileMenuSlideDuration = fileMenuSlideDuration;
	emit fileMenuSlideDurationChanged(_fileMenuSlideDuration);
}

void JaspTheme::setToolTipDelay(theme_timeType toolTipDelay)
{
	if (_toolTipDelay == toolTipDelay)
		return;

	_toolTipDelay = toolTipDelay;
	emit toolTipDelayChanged(_toolTipDelay);
}

void JaspTheme::setToolTipTimeout(theme_timeType toolTipTimeout)
{
	if (_toolTipTimeout == toolTipTimeout)
		return;

	_toolTipTimeout = toolTipTimeout;
	emit toolTipTimeoutChanged(_toolTipTimeout);
}

void JaspTheme::setFont(QFont font)
{
	if (_font == font)
		return;

	_font = font;
	emit fontChanged(_font);
}

void JaspTheme::setFontLabel(QFont fontLabel)
{
	if (_fontLabel == fontLabel)
		return;

	_fontLabel = fontLabel;
	emit fontLabelChanged(_fontLabel);
}

void JaspTheme::setFontRibbon(QFont fontRibbon)
{
	if (_fontRibbon == fontRibbon)
		return;

	_fontRibbon = fontRibbon;
	emit fontRibbonChanged(_fontRibbon);
}

void JaspTheme::setFontGroupTitle(QFont fontGroupTitle)
{
	if (_fontGroupTitle == fontGroupTitle)
		return;

	_fontGroupTitle = fontGroupTitle;
	emit fontGroupTitleChanged(_fontGroupTitle);
}

void JaspTheme::setFontPrefOptionsGroupTitle(QFont fontPrefOptionsGroupTitle)
{
	if (_fontPrefOptionsGroupTitle == fontPrefOptionsGroupTitle)
		return;

	_fontPrefOptionsGroupTitle = fontPrefOptionsGroupTitle;
	emit fontPrefOptionsGroupTitleChanged(_fontPrefOptionsGroupTitle);
}

void JaspTheme::setIconPath(QString iconPath)
{
	if (_iconPath == iconPath)
		return;

	_iconPath = iconPath;
	emit iconPathChanged(_iconPath);
}

void JaspTheme::setThemeName(QString themeName)
{
	if (_themeName == themeName)
		return;

	if(themeName != "")
	{
		//Log::log() << "Inserting theme with name " << themeName << " into _themes" << std::endl;

		if(_themes.count(_themeName) > 0 && _themes[_themeName] == this)
			_themes.erase(_themeName);

		if(_themes.count(themeName) == 0) //If there is space now, then insert it. Otherwise there was already a theme there with this name and that means we might still be going down the derivatory descent
			_themes[themeName] = this;
	}

	_themeName = themeName;
	emit themeNameChanged(_themeName);

	setIconPath("qrc:/icons/" + _themeName + "/");

	if(_currentTheme == this)
		emit currentThemeNameChanged();
}

void JaspTheme::setFontRCode(QFont fontRCode)
{
	if (_fontRCode == fontRCode)
		return;

	_fontRCode = fontRCode;
	emit fontRCodeChanged(_fontRCode);
}

void JaspTheme::setFontCode(QFont fontCode)
{
	if (_fontCode == fontCode)
		return;

	_fontCode = fontCode;
	emit fontCodeChanged(_fontCode);
}

void JaspTheme::setIsDark(bool isDark)
{
	if (_isDark == isDark)
		return;

	_isDark = isDark;
	emit isDarkChanged(_isDark);
}

void JaspTheme::uiScaleHandler(float newUiScale)
{
	_uiScale = newUiScale;
	emit uiScaleChanged(_uiScale);
}

void JaspTheme::maxFlickVeloHandler(float maxFlickVelo)
{
	_maximumFlickVelocity = maxFlickVelo;
	emit maximumFlickVelocityChanged();
}

QString JaspTheme::currentIconPath()
{
	if(_currentTheme)
		return _currentTheme->iconPath();
	return "qrc:/icons/";
}

void JaspTheme::setDarkeningColour(QColor darkeningColour)
{
	if (_darkeningColour == darkeningColour)
		return;

	_darkeningColour = darkeningColour;
	emit darkeningColourChanged(_darkeningColour);
}

void JaspTheme::updateFontMetrics()
{
	if(currentTheme())
		_fontMetrics = QFontMetricsF(currentTheme()->font());
}
