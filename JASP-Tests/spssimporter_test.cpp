//
// Copyright (C) 2016 University of Amsterdam
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

#pragma once
#include "spssimporter_test.h"
#include "csviterator.h"


void SPSSImporterTest::initTestCase()
{

}

void SPSSImporterTest::cleanupTestCase()
{

}

void SPSSImporterTest::init()
{
  fe = new FileEvent();
  dsp = new DataSetPackage();
  asl = new AsyncLoader();

  fe2 = new FileEvent();
  dsp2 = new DataSetPackage();
  asl2 = new AsyncLoader();
}

void SPSSImporterTest::cleanup()
{
  // destroy all the objects created and delete the dataSet from the shared memory
  //SharedMemory::deleteDataSet(dsp->dataSet);
  //SharedMemory::deleteDataSet(dsp2->dataSet);

  fe->~FileEvent();
  dsp->~DataSetPackage();
  asl->~AsyncLoader();

  fe2->~FileEvent();
  dsp2->~DataSetPackage();
  asl2->~AsyncLoader();  
}

void SPSSImporterTest::spssTester_data()
{
  QTest::addColumn<QString>("filename");
  int count = 0;

  boost::filesystem::path p("test_files/spss_files");

  //add files to be tested in a folder "test_files"
  for (auto i = boost::filesystem::directory_iterator(p); i != boost::filesystem::directory_iterator(); i++)
  {
    if (!boost::filesystem::is_directory(i->path())) //we eliminate directories
    {
      QTest::newRow("spss file test") << QString::fromStdString(i->path().filename().string());
      count++;
    }
  }
}


void SPSSImporterTest::spssTester()
{
  QFETCH(QString, filename);

  //spss file
  QString full_path1 = QString("test_files/spss_files/").append(filename);
  qDebug() << "filename: " << filename;

  DataSetPackage *ds_spss = new DataSetPackage();
  fe->setOperation(FileEvent::FileOpen);
  fe->setPath(full_path1);
  asl->loadTask(fe, ds_spss);
  asl->_thread.quit();

  struct fileContent fc1;
  copyToStructure(ds_spss, &fc1);
  SharedMemory::deleteDataSet(ds_spss->dataSet);
  ds_spss->~DataSetPackage();


  //csv file
  QString csvFile = filename;
  csvFile.replace(filename.size()-3, 3, "csv");
  QString full_path2 = QString("test_files/csv_files/").append(csvFile);  
  qDebug() << "csv filename: " << csvFile;

  DataSetPackage *ds_csv = new DataSetPackage();
  fe2->setOperation(FileEvent::FileOpen);
  fe2->setPath(full_path2);
  asl2->loadTask(fe2, ds_csv);
  asl2->_thread.quit();

  struct fileContent fc2;
  copyToStructure(ds_csv, &fc2);
  SharedMemory::deleteDataSet(ds_csv->dataSet);
  ds_csv->~DataSetPackage();


  bool ans = checkIfEqual(&fc1, &fc2);
  QVERIFY(ans); // end of test  
}

void SPSSImporterTest::copyToStructure(DataSetPackage *dsPackage, struct fileContent *fc)
{
  fc->columns = dsPackage->dataSet->columnCount();//copy column count
  fc->rows = dsPackage->dataSet->rowCount();      //copy row count

  //headers copy
  std::vector<std::string> headerNames;
  for(int i=0; i<fc->columns; ++i)
  {
    headerNames.push_back(dsPackage->dataSet->column(i).name());
  }
  fc->headers = headerNames;

  //copy data
  std::vector< std::vector<std::string> > fileRows;
  for(int j=0; j<fc->rows; ++j)
  {
    std::vector<std::string> tempRow;
    for(int i=0; i<fc->columns; ++i)
    {
      tempRow.push_back(dsPackage->dataSet->column(i)[j]);
    }
    fileRows.push_back(tempRow);
    tempRow.clear();
  }

  fc->data = fileRows;
  return;
}


/* checks if data read from file is same as the data stored in the shared memory */
bool SPSSImporterTest::checkIfEqual(struct fileContent *fc1, struct fileContent *fc2)
{
  if(fc1->columns != fc2->columns)
  {
    qDebug() << "Column size mismatch";
    return false;
  }

  if(fc1->rows != fc2->rows)
  {
    qDebug() << "Row size mismatch" << QString::number(fc1->rows) << " " << QString::number(fc2->rows);
    //return false;
  }

  for(int i=0; i<fc2->columns; ++i)
  {
    if(fc1->headers[i] != fc2->headers[i])
    {
      qDebug() << "Header name mismatch";
      return false;
    }

    for(int j=0; j<fc2->rows; ++j)
    {

      if(fc1->data[j][i] != fc2->data[j][i])
      {qDebug() << "Data mismatch " << QString::number(j);
        qDebug() << QString::fromStdString(fc2->data[j][i])<< " " << QString::fromStdString(fc1->data[j][i]);
        return false;
      }
    }
  }

  return true;
}

/* read data from the file specified from path and store it in the struct fileContent */
int SPSSImporterTest::readDataFromCSV(QString path, struct fileContent *fc)
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
      if((*csvIter).size() == 1 && trim((*csvIter)[0]) == "")
      {
        continue;
      }
      for(int i=0; i<(*csvIter).size(); ++i)
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
    qDebug() << "Unable to open file: " << path;
    return 1;
  }
}

std::string SPSSImporterTest::roundTo6Digits(double x, int n)
{ 
  char buff[32];
  sprintf(buff, "%.*g", n, x);
  std::string cppString(buff);
  return cppString;
}

bool SPSSImporterTest::checkIfNumeric(std::string word)
{
    std::string::const_iterator it = word.begin();
    bool decimalPoint = false;
    int minimumSize = 0;

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
