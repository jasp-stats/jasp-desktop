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

#include "textfileread_test.h"


void TextFileReadTest::initTestCase()
{
  if(boost::filesystem::exists(TESTFILE_FOLDER "textfileread_test"))
  {
    folderPathFound = true;
  }
  else
  {
    folderPathFound = false;
  }
}

void TextFileReadTest::cleanupTestCase()
{

}

void TextFileReadTest::init()
{
  fe = new FileEvent();
  dsp = new DataSetPackage();
  asl = new AsyncLoader();
}

void TextFileReadTest::cleanup()
{
  if(dsp->dataSet != NULL)
  {
    // destroy all the objects created and delete the dataSet from the shared memory
    SharedMemory::deleteDataSet(dsp->dataSet);
  }

  fe->~FileEvent();
  dsp->~DataSetPackage();
  asl->~AsyncLoader();
}

void TextFileReadTest::asyncloaderTester_data()
{
  if(folderPathFound)
  {
    QTest::addColumn<QString>("filename");
    boost::filesystem::path _path(TESTFILE_FOLDER "textfileread_test");

    //add files to be tested in a folder "Resources/TestFiles/spssimporter_test/spss_files"
    for (auto i = boost::filesystem::directory_iterator(_path); i != boost::filesystem::directory_iterator(); i++)
    {
      if (!boost::filesystem::is_directory(i->path())) //we eliminate directories
      {
        QTest::newRow("text file-read test") << QString::fromStdString(i->path().filename().string());
      }
    }
  }
}


void TextFileReadTest::asyncloaderTester()
{

  if(folderPathFound)
  {
    QFETCH(QString, filename);
    qDebug() << "File: " << filename;

    //text file open
    QString folderPath = TESTFILE_FOLDER "textfileread_test/";
    QString _path = folderPath.append(filename);

    struct fileContent fc;
    int error = readDataFromFile(_path.toUtf8().constData(), &fc);

    if(error)
    {
      QVERIFY2(false, "File not found");        //file open failed
    }
    else
    {
      bool wasBlocked = fe->blockSignals(true); //block all signals emitted by the FileEvent object
      fe->setPath(_path);

      wasBlocked = asl->blockSignals(true);     //block all signals emitted by the Asyncloader object
      asl->loadTask(fe, dsp);
      asl->_thread.quit();

      QVERIFY(checkIfEqual(&fc));               //test the opening and reading of text files
    }
  }
  else
  {
    QVERIFY2(false, "Folder path not found");
  }
}


/* checks if data read from file is same as the data stored in the shared memory */
bool TextFileReadTest::checkIfEqual(struct fileContent *fc)
{
  if(fc->columns != dsp->dataSet->columnCount())
  {
    return false;
  }

  if(fc->rows != dsp->dataSet->rowCount())
  {
    return false;
  }

  for(int i=0; i<fc->columns; ++i)
  {
    if(fc->headers[i] != dsp->dataSet->column(i).name())
    {
      return false;
    }

    for(int j=0; j<fc->rows; ++j)
    {
      if(fc->data[j][i] != dsp->dataSet->column(i)[j])
      {
        return false;
      }
    }
  }

  return true;
}

/* read data from the file specified from path and store it in the struct fileContent */
int TextFileReadTest::readDataFromFile(std::string path, struct fileContent *fc)
{
  std::ifstream input(path.c_str());
  std::vector< std::vector<std::string> > fileRows;
  
  int numCols = 0;
  int numRows = 0;
  char delimiter = '\t';

  if(input.is_open())
  {
    std::string line; //line from the file
    std::string currentWord;
    std::vector<std::string> tempRow;

    std::getline(input, line);
    std::size_t found = line.find(delimiter);

    if(found == std::string::npos) // tab is not found, separater is space character
    {
      delimiter = ' ';
    }

    std::istringstream buffer(line);

    while(std::getline(buffer, currentWord, delimiter)) //separate with respect to the delimiter
    {
      numCols++;
      tempRow.push_back(currentWord);
    }

    fc->columns = numCols;
    fc->headers = tempRow;
    buffer.clear();
    tempRow.clear();

    while(std::getline(input, line))
    {
      numRows++;
      buffer.str(line);

      int numWordsCurrent = 1;
	  for(size_t i=0; i<line.size(); ++i)
      {
        if(line[i]==delimiter)
        {
          numWordsCurrent++;
        }
      }

      for(int i=0; i<numWordsCurrent; ++i)
      {
        std::getline(buffer, currentWord, delimiter);

        bool valid = false;
        //check if current word has letters/numbers
		for(size_t j=0; j<currentWord.size(); ++j)
        {
          if(currentWord[j] != ' ' && currentWord[j] != '\t')
          {
            valid = true;
            break;
          }
        }
        if(valid)
        {

          tempRow.push_back(currentWord);
        }
        else
        {
          tempRow.push_back(".");
        }
      }

      for(int i=numWordsCurrent; i<numCols; ++i)//fill remaining with '.'
      {
        tempRow.push_back(".");
      }

      fileRows.push_back(tempRow);
      tempRow.clear();
      buffer.clear();
    }

    fc->rows = numRows;
    fc->data = fileRows;

    return 0;
  }
  else
  {
    qDebug() << "File open failed";

    return 1;
  }
}
