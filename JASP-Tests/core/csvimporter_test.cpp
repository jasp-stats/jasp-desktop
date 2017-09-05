//
// Copyright (C) 2017 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#include "csvimporter_test.h"
#include "csviterator.h"


void CSVImporterTest::initTestCase()
{

}

void CSVImporterTest::cleanupTestCase()
{

}

void CSVImporterTest::init()
{
  fe = new FileEvent();
  dsp = new DataSetPackage();
  asl = new AsyncLoader();
}

void CSVImporterTest::cleanup()
{
  // destroy all the objects created and delete the dataSet from the shared memory
  SharedMemory::deleteDataSet(dsp->dataSet);

  fe->~FileEvent();
  dsp->~DataSetPackage();
  asl->~AsyncLoader();
}

void CSVImporterTest::csvTester_data()
{
  QTest::addColumn<QString>("filename");
  int count = 0;

  boost::filesystem::path p(TESTFILE_FOLDER "csvimporter_test");

  //add files to be tested in a folder "Resources/TestFiles/csvimporter_test"
  for (auto i = boost::filesystem::directory_iterator(p); i != boost::filesystem::directory_iterator(); i++)
  {
    if (!boost::filesystem::is_directory(i->path())) //we eliminate directories
    {
      QTest::newRow("csv file test") << QString::fromStdString(i->path().filename().string());
      count++;
    }
  }
}


void CSVImporterTest::csvTester()
{
  QFETCH(QString, filename);

  qDebug() << "filename: " << filename;

  QString full_path = QString(TESTFILE_FOLDER "csvimporter_test/").append(filename);

  fe->setPath(full_path);

  asl->loadTask(fe, dsp);
  asl->_thread.quit();

  columnIsNumeric.resize(dsp->dataSet->columnCount()); //set default column type as numeric
  for(int i=0; i<dsp->dataSet->columnCount(); ++i)
  {
    columnIsNumeric[i] = true;
  }

  struct fileContent fc;
  int error = readDataFromCSV(full_path, &fc);

  if(error == 1) //file could not be opened
  {
    QVERIFY(false);
  }
  else
  {
    bool ans = checkIfEqual(&fc);
    QVERIFY(ans);
  }
}


/* checks if data read from file is same as the data stored in the shared memory */
bool CSVImporterTest::checkIfEqual(struct fileContent *fc)
{
  if(fc->columns != dsp->dataSet->columnCount())
  {
    qDebug() << "Column size mismatch";
    return false;
  }

  if(fc->rows != dsp->dataSet->rowCount())
  {
    qDebug() << "Row size mismatch" << QString::number(fc->rows) << " " << QString::number(dsp->dataSet->rowCount());
    return false;
  }

  for(int i=0; i<fc->columns; ++i)
  {
    if(fc->headers[i] != dsp->dataSet->column(i).name())
    {
      qDebug() << "Header name mismatch";
      return false;
    }

    for(int j=0; j<fc->rows; ++j)
    {
      std::string currentWord = fc->data[j][i];

      if(columnIsNumeric[i] && currentWord!= ".")
      {
        double temp = ::atof(currentWord.c_str());
        currentWord = roundTo6Digits(temp, 6);
      }

      if(currentWord != dsp->dataSet->column(i)[j])
      {
        qDebug() << "Data mismatch " << QString::fromStdString(currentWord)<< " " << QString::fromStdString(dsp->dataSet->column(i)[j]);
        return false;
      }
    }
  }

  return true;
}

/* read data from the file specified from path and store it in the struct fileContent */
int CSVImporterTest::readDataFromCSV(QString path, struct fileContent *fc)
{
  std::ifstream input(path.toStdString().c_str());
  std::vector< std::vector<std::string> > fileRows;
  std::string currentWord;

  if(input.is_open())
  {
    for(CSVIterator csvIter(input); csvIter != CSVIterator(); ++csvIter)
    {
      std::vector<std::string> tempRow; //has one row

      if((*csvIter).size() <=0)
      {
        continue;
      }
	  for(size_t i=0; i<(*csvIter).size(); ++i)
      {
        currentWord = (*csvIter)[i];
        if(currentWord == "")
        {
          currentWord = ".";
        }
        else
        {
          if(!fileRows.empty())
          {
            if(columnIsNumeric[i])//check if the column has strings that are non-nueric
            {
              if(!checkIfNumeric(currentWord)) //check if the currentWord is numeric
              {
                columnIsNumeric[i] = false;
              }
            }
          }
        }

        tempRow.push_back(currentWord);
      }

      fileRows.push_back(tempRow);
      tempRow.clear();
    }

    fc->rows = fileRows.size() - 1;
    fc->columns = fileRows[0].size();
    fc->headers = fileRows[0];
    fileRows.erase(fileRows.begin());
    fc->data = fileRows;

    return 0;
  }
  else
  {
    qDebug() << "Unable to open file";
    return 1;
  }
}

std::string CSVImporterTest::roundTo6Digits(double x, int n)
{ 
  char buff[32];
  sprintf(buff, "%.*g", n, x);
  std::string cppString(buff);
  return cppString;
}

bool CSVImporterTest::checkIfNumeric(std::string word)
{
    std::string::const_iterator it = word.begin();
    bool decimalPoint = false;
	size_t minimumSize = 0;

    if( word.size()>0 && ( word[0] == '-' || word[0] == '+' ) )
    {
      it++;
      minimumSize++;
    }

    while(it != word.end())
    {
      if(*it == '.')
      {
        if(!decimalPoint)
        {
          decimalPoint = true;
        }
        else
        {
          break;
        }
      }
      else if( !std::isdigit(*it) && ( (*it!='f') || it+1 != word.end() || !decimalPoint ) )
      {
        break;
      }

      ++it;
    }

    return (word.size()>minimumSize && it == word.end());
}
