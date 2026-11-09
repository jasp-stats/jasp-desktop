import QtQuick
import QtTest
import JASP.Controls

// Without Desktop's PreferencesModel (syntax bridge, QuickTest) the QML `preferencesModel` is a
// PreferencesModelBase. Module QML reads preferences from it (jaspReliability's hidden
// `hiddenScaleThreshold`, jaspPredictiveAnalytics' `reportingMode`), so it must offer them with Desktop's defaults.
// TestCase functions run alphabetically: the defaults are checked (test_a...) before test_b... mutates and restores them.
TestCase
{
	name:		"TestPreferencesModelDefaults"

	property alias form: jaspForm

	Form
	{
		id: jaspForm

		IntegerField
		{
			id:				hiddenScaleThreshold
			name:			"hiddenScaleThreshold"
			visible:		false
			defaultValue:	preferencesModel.thresholdScale
		}

		CheckBox
		{
			id:				reportingCheck
			name:			"reportingCheck"
			enabled:		preferencesModel.reportingMode
		}
	}

	function test_a_desktopDefaults()
	{
		compare(preferencesModel.thresholdScale,	10,		"thresholdScale defaults to Desktop's ThresholdScale setting");
		compare(preferencesModel.reportingMode,		false,	"reportingMode defaults to off");
		compare(hiddenScaleThreshold.value,			10);
		compare(hiddenScaleThreshold.boundJson(),	10);
		compare(reportingCheck.enabled,				false);
	}

	function test_b_thresholdScaleNotifies()
	{
		var original = preferencesModel.thresholdScale;
		try
		{
			preferencesModel.thresholdScale = 7;
			compare(preferencesModel.thresholdScale,	7);
			compare(hiddenScaleThreshold.value,			7, "the bound default follows thresholdScaleChanged");

			preferencesModel.reportingMode = true;
			compare(reportingCheck.enabled,				true, "enabled follows reportingModeChanged");
		}
		finally
		{
			preferencesModel.thresholdScale	= original;
			preferencesModel.reportingMode	= false;
		}
	}
}
