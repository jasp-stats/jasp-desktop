import QtTest
import QtQuick
import QtWebEngine
import QtWebChannel
import QtQuick.Controls
//import JASP.Controls		as JC


TestCase
{
		
	WebEngineView
	{
		id:						resultsView

		url:					"qrc:///html/index-jasp.html"
		
		signal jsDone(result: var);
		signal resultsLoaded();
		
		onLoadingChanged: (loadRequest)=>
		{
			if(loadRequest.status === WebEngineView.LoadSucceededStatus)
				resultsLoaded();
		}
		
	}
	
	SignalSpy
	{
		id:				spyLoader
		target:			resultsView
		signalName:		"resultsLoaded"
	}
	
	SignalSpy
	{
		id:				spy
		target:			resultsView
		signalName:		"jsDone"
	}

	function test_format()
	{
		//Make sure the resultspage loads
		spyLoader.wait(1000);
		compare(spyLoader.count, 1);
		
		
		resultsView.runJavaScript(	"let column=[{content: 1.234}];" +
									"let result = formatColumn(column=column, type='number', format='sf:4', alignNumbers=false, combine=false, modelFootnotes='');" +
									"result[0].content",
								  function(result) { resultsView.jsDone(result); });
		
		spy.wait(1000);
		compare(spy.count, 1);
		//console.log(spy.signalArguments[0][0])
		compare(spy.signalArguments[0][0], "1.234");
		
		spy.clear();
		resultsView.runJavaScript(	"result = formatColumn(column=column, type='number', format='sf:2', alignNumbers=false, combine=false, modelFootnotes='');" +
									"result[0].content",
								  function(result) { resultsView.jsDone(result); });
		spy.wait(1000);
		compare(spy.count, 1);
		compare(spy.signalArguments[0][0], "1.2");
		
		
		spy.clear()
		resultsView.runJavaScript(	"result = formatColumn(column=column, type='number', format='sf:4', alignNumbers=true, combine=false, modelFootnotes='');" +
									"result[0].content",
								  function(result) { resultsView.jsDone(result); });
		
		spy.wait(1000);
		compare(spy.count, 1);
		compare(spy.signalArguments[0][0], "1.234");
		
		spy.clear()
		resultsView.runJavaScript(	"column=[{content: 123456789}];" +
									"result = formatColumn(column=column, type='number', format='sf:3', alignNumbers=true, combine=false, modelFootnotes='');" +
									"result[0].content",
									function(result) { resultsView.jsDone(result); });
		
		spy.wait(1000);
		compare(spy.count, 1);
		compare(spy.signalArguments[0][0], "1.23&times;10<sup>+8</sup>");
		
		spy.clear()
		resultsView.runJavaScript(	"column=[{content: 1.234}];" +
									"result = formatColumn(column=column, type='number', format='dp:3', alignNumbers=true, combine=false, modelFootnotes='');" +
									"result[0].content",
									function(result) { resultsView.jsDone(result); });
		
		spy.wait(1000);
		compare(spy.count, 1);
		compare(spy.signalArguments[0][0], "1.234");
		
		
		spy.clear()
		resultsView.runJavaScript(	"result = formatColumn(column=column, type='number', format='dp:1', alignNumbers=true, combine=false, modelFootnotes='');" +
									"result[0].content",
								  function(result) { resultsView.jsDone(result); });
		
		spy.wait(1000);
		compare(spy.count, 1);
		compare(spy.signalArguments[0][0], "1.2");
		
		
		spy.clear()
		resultsView.runJavaScript(	"column=[{content: 0.9999}];" +
									"result = formatColumn(column=column, type='number', format='p:0.01;dp:3', alignNumbers=true, combine=false, modelFootnotes='');" +
									"result[0].content",
								  function(result) { resultsView.jsDone(result); });
		
		spy.wait(1000);
		compare(spy.count, 1);
		compare(spy.signalArguments[0][0], "1.000");
		
		
		spy.clear()
		resultsView.runJavaScript(	"column=[{content: 0.9}];" +
									"result = formatColumn(column=column, type='number', format='p:0.01;dp:3', alignNumbers=true, combine=false, modelFootnotes='');" +
									"result[0].content",
								  function(result) { resultsView.jsDone(result); });
		
		spy.wait(1000);
		compare(spy.count, 1);
		compare(spy.signalArguments[0][0], ".900");
		
		
		spy.clear()
		resultsView.runJavaScript(	"column=[{content: 1000234}];" +
									"setCurrentLocaleID('en', false);"+
									"result = formatColumn(column=column, type='number', format='monetary:EUR;thousands', alignNumbers=true, combine=false, modelFootnotes='');" +
									"result[0].content",
								  function(result) { resultsView.jsDone(result); });
		
		spy.wait(1000);
		compare(spy.count, 1);
		compare(spy.signalArguments[0][0], "€1,000,234");
		
		spy.clear()
		resultsView.runJavaScript(	"column=[{content: 1000234}];" +
									"setCurrentLocaleID('en', false);"+
									"result = formatColumn(column=column, type='number', format='monetary:EUR', alignNumbers=true, combine=false, modelFootnotes='');" +
									"result[0].content",
								  function(result) { resultsView.jsDone(result); });
		
		spy.wait(1000);
		compare(spy.count, 1);
		compare(spy.signalArguments[0][0], "€1000234");
		
		spy.clear()
		resultsView.runJavaScript(	"column=[{content: 1000234}];" +
									"setCurrentLocaleID('en', true);"+
									"result = formatColumn(column=column, type='number', format='monetary:EUR', alignNumbers=true, combine=false, modelFootnotes='');" +
									"result[0].content",
								  function(result) { resultsView.jsDone(result); });
		
		spy.wait(1000);
		compare(spy.count, 1);
		compare(spy.signalArguments[0][0], "€1,000,234");
		
		
		spy.clear()
		resultsView.runJavaScript(	"column=[{content: 1.01}];" +
									"result = formatColumn(column=column, type='number', format='pc', alignNumbers=true, combine=false, modelFootnotes='');" +
									"result[0].content",
								  function(result) { resultsView.jsDone(result); });
		
		spy.wait(1000);
		compare(spy.count, 1);
		compare(spy.signalArguments[0][0], "101.00&thinsp;%");
		
		
		spy.clear()
		resultsView.runJavaScript(	"column=[{content: 0.01}];" +
									"result = formatColumn(column=column, type='number', format='pc', alignNumbers=true, combine=false, modelFootnotes='');" +
									"result[0].content",
								  function(result) { resultsView.jsDone(result); });
		
		spy.wait(1000);
		compare(spy.count, 1);
		compare(spy.signalArguments[0][0], "1.00&thinsp;%");
		
		spy.clear()
		resultsView.runJavaScript(	"column=[{content: 0.9999}];" +
									"result = formatColumn(column=column, type='number', format='pc', alignNumbers=true, combine=false, modelFootnotes='');" +
									"result[0].content",
								  function(result) { resultsView.jsDone(result); });
		
		spy.wait(1000);
		compare(spy.count, 1);
		compare(spy.signalArguments[0][0], "99.99&thinsp;%");
	}

	function test_utils_comprehensive()
	{
		// Page was already loaded by test_format, no need to wait
		compare(spyLoader.count, 1);

		spy.clear();
		resultsView.runJavaScript(
			"var r = runAllUtilTests();" +
			"JSON.stringify(r);",
			function(result) { resultsView.jsDone(result); }
		);

		spy.wait(1000);
		compare(spy.count, 1);

		var parsed = JSON.parse(spy.signalArguments[0][0]);

		console.log("Utils tests: " + parsed.passed + " passed, " + parsed.failed + " failed, " + parsed.total + " total");

		if (parsed.failed > 0) {
			for (var i = 0; i < parsed.results.length; i++) {
				var r = parsed.results[i];
				if (!r.passed)
					console.log("  FAIL: " + r.test + " | expected: '" + r.expected + "' | actual: '" + r.actual + "'");
			}
		}

		compare(parsed.failed, 0, "All util tests should pass (" + parsed.total + " total)");
	}

}




//function formatColumn(column, type, format, alignNumbers, combine, modelFootnotes, html = true, errorOnMixed = false) {
	/**
	 * Prepares the columns of a table to the required format
	 * @param column
	 * @param type           column type - string, number, pvalue, integer, ...
	 * @param format         decimal format, p-value format
	 * @param alignNumbers
	 * @param combine
	 * @param modelFootnotes
	 * @param html           html render or latex code. Default is true
	 * @param errorOnMixed   internal, used to avoid infinite loops
	 */
