#include "compareresults.h"
#include <QXmlStreamReader>
#include <stack>
#include <iostream>
#include "log.h"
#include <QFile>
#include <QTextStream>
#include <QFileInfo>
#include "analysis/analyses.h"

namespace resultXmlCompare
{

compareResults * compareResults::singleton = nullptr;

compareResults * compareResults::theOne()
{
	if(singleton == nullptr)
		singleton = new compareResults();

	return singleton;
}

void compareResults::sanitizeHtml(QString & result)
{
	// If there are other elements that need to be replaced this function probably should be rewritten somewhat intelligently with a map or a vector or something.

	std::vector<std::pair<QString, QString>> replacers (
		{
			std::make_pair("&nbsp;",	" "),
			std::make_pair("&tau;",		"tau"),
			std::make_pair("<br>",		"\n"),
			std::make_pair("\"Segoe UI\"", ""), //Why are we replacing these fonts anyway?
			std::make_pair("\"Helvetica Neue\"", ""),
			std::make_pair("\"Lucida Grande\"", "")
		});

	for(auto & p : replacers)
		result = result.replace(p.first, p.second);
}

void compareResults::setOriginalResult(QString result)
{
	originalResultExport  = result;
	sanitizeHtml(originalResultExport);
}

void compareResults::setRefreshResult(QString result)
{
	refreshedResultExport = result;
	sanitizeHtml(refreshedResultExport);
}

bool compareResults::checkForAnalysisError()
{
	Analyses::analyses()->applyToSome([&](Analysis * a) -> bool
	{
		if(a->status() == Analysis::Status::FatalError || a->status() == Analysis::Status::ValidationError)
		{
			_analysisHadError = true;
			return false;
		}
		else
			return true;
	});

	return _analysisHadError;
}

result compareResults::convertXmltoResultStruct(const QString &  resultXml)
{
	QXmlStreamReader xml(resultXml);

	enum class stateType { nothing, result, table, head, body, foot, row, headercell, bodycell, h2 };

	typedef QXmlStreamReader::TokenType ttype;
	typedef std::stack<stateType>		typeStack;

	result res;
	typeStack levels;
	levels.push(stateType::result);

	auto curLinePos = [&](){ return "(line: " + std::to_string(xml.lineNumber()) + " col: " + std::to_string(xml.columnNumber()) + ")"; };
	auto curBlock	= [&]()
	{
		typeStack		levelCopy			= levels;
		tableBlock *	returnMe			= res.curTable().body(); // body is default

		auto			checkSaveAndPopTop	= [&](stateType checkMe)
		{
			bool	isType = levelCopy.top() == checkMe;

			if(isType)
				levelCopy.pop();
			return isType;
		};

		checkSaveAndPopTop(stateType::headercell);
		checkSaveAndPopTop(stateType::bodycell);
		checkSaveAndPopTop(stateType::row);

		if(levelCopy.top() == stateType::head)
			returnMe = res.curTable().head();
		else if(levelCopy.top() == stateType::foot)
			returnMe = res.curTable().foot();

		return returnMe;
	};

	auto topIs = [&](stateType checkMe) { return levels.top() == checkMe; };

	while (!xml.atEnd())
	{
		switch(xml.readNext())
		{
		case ttype::StartElement:
		{
			auto elementName = xml.name().toString().toStdString();

			if(elementName == "table")
			{
				res.genTable();
				levels.push(stateType::table);
			}

			if(elementName == "thead")
			{
				if(!topIs(stateType::table))
					std::cerr << "found table head " << curLinePos() << " outside of table!" << std::endl;

				levels.push(stateType::head);
			}

			if(elementName == "tbody")
			{
				if(!topIs(stateType::table))
					std::cerr << "found table body " << curLinePos() << " outside of table!" << std::endl;

				levels.push(stateType::body);
			}

			if(elementName == "tfoot")
			{
				if(!topIs(stateType::table))
					std::cerr << "found table footer " << curLinePos() << " outside of footer!" << std::endl;

				levels.push(stateType::foot);
			}

			if(elementName == "tr")
			{
				if(!(topIs(stateType::head) || topIs(stateType::body) || topIs(stateType::foot)))
					std::cerr << "found row " << curLinePos() << " outside of table head, body or footer!" << std::endl;

				levels.push(stateType::row);

				curBlock()->genRow();
			}

			if(elementName == "th")
			{
				if(!topIs(stateType::row))
					std::cerr << "found headercell " << curLinePos() << " outside of row!" << std::endl;

				levels.push(stateType::headercell);

				curBlock()->curRow().genHeaderCell();
			}

			if(elementName == "td")
			{
				if(!topIs(stateType::row))
					std::cerr << "found bodycell " << curLinePos() << " outside of row!" << std::endl;

				levels.push(stateType::bodycell);

				curBlock()->curRow().genBodyCell();
			}

			if(elementName == "h2")
				levels.push(stateType::h2);
			break;
		}

		case ttype::Characters:
			if(!xml.isWhitespace())
			{
				std::string text(xml.text().toString().trimmed().toStdString());

				if(topIs(stateType::h2))
					if(text == "error")
						res.setError();

				if(topIs(stateType::headercell) || topIs(stateType::bodycell))
				{
					tableBlock * curBlk = curBlock();
					curBlk->curRow().curCell().addToValue(text);
				}
			}
			break;

		case ttype::EndElement:
		{
			auto elementName = xml.name().toString().toStdString();

			if(elementName == "th")
			{
				if(!topIs(stateType::headercell))
					std::cerr << "found end of headercell " << curLinePos() << " outside of headercell!" << std::endl;
				levels.pop();
			}

			if(elementName == "td")
			{
				if(!topIs(stateType::bodycell))
					std::cerr << "found end of bodycell " << curLinePos() << " outside of bodycell!" << std::endl;
				levels.pop();
			}

			if(elementName == "tr")
			{
				if(!topIs(stateType::row))
					std::cerr << "found end of row " << curLinePos() << " outside of row!" << std::endl;
				levels.pop();
			}

			if(elementName == "thead")
			{
				if(!topIs(stateType::head))
					std::cerr << "found table head " << curLinePos() << " outside of table!" << std::endl;
				levels.pop();
			}

			if(elementName == "tbody")
			{
				if(!topIs(stateType::body))
					std::cerr << "found end of body of table " << curLinePos() << " outside of body!" << std::endl;
				levels.pop();
			}

			if(elementName == "tfoot")
			{
				if(!topIs(stateType::foot))
					std::cerr << "found end of footer of table " << curLinePos() << " outside of footer!" << std::endl;
				levels.pop();
			}

			if(elementName == "table")
			{
				if(!topIs(stateType::table))
					std::cerr << "found end of table " << curLinePos() << " outside of table!" << std::endl;
				levels.pop();
			}

			if(elementName == "h2")
			{
				if(!topIs(stateType::h2))
					std::cerr << "found end of h2 " << curLinePos() << " outside of h2" << std::endl;
				levels.pop();
			}
			break;
		}

		default:
			//ignore
			break;
		}
	}

	if(xml.hasError())
	{
		std::cerr << "xml hadError: " << xml.errorString().toStdString() << "!" << std::endl;
		//const char * xmlDivider = "<---------------------------------------------------------------------------------------------------------------------------------------------------------------------->\n";
		//std::cerr << "broken XML:\n" << xmlDivider << resultXml.toStdString() << xmlDivider << std::endl;
	}

	return res;
}

bool compareResults::compare()
{
	return compare(originalResultExport, refreshedResultExport);
}

bool compareResults::compare(const QString & resultOld, const QString & resultNew)
{
	ranCompare = true;

	std::cout << "Old result conversion:" << std::endl;
	/*QFile file("out.txt");
	if (!file.open(QIODevice::WriteOnly | QIODevice::Text))
		return false;
	QFileInfo fileInfo(file);
	QString path = fileInfo.absolutePath();

	QTextStream out(&file);
	out << "hallo";
	out << "Old result:\n" << resultOld << "\n";
	out << "Old result conversion:" << "\n"; */
	result oldRes = convertXmltoResultStruct(resultOld);
	std::cout << "\nConverted to:\n" << oldRes.toString() << "" << std::endl;

	std::cout << "New result conversion:" << std::endl;
	//out << "New result\n" << resultNew << "\n" << "\n";
	//out << "New result conversion:" << "\n";
	result newRes = convertXmltoResultStruct(resultNew);
	std::cout << "\nConverted to:\n" << newRes.toString() << "" << std::endl;
	//file.close():

	succes = oldRes == newRes;

	std::stringstream compareConclusion;
	compareConclusion << "The results are " << (succes ? "the same!" : "different...") << "\n";

	if(!succes)
		compareConclusion << oldRes.diffToString(newRes) << "\n";

	std::cerr  << compareConclusion.str() << std::endl;
	Log::log() << compareConclusion.str();
	return succes;
}


}
