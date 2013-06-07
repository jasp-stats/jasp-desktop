#include "datasetloader.h"

#include "dataset.h"
#include "csvparser.h"

#include <QDebug>

using namespace boost::interprocess;

DataSetLoader::DataSetLoader()
{
}

DataSet* DataSetLoader::loadFile(istream &is) {

	CSVParser parser;
	std::vector<string> columns = std::vector<string>();
	std::vector< std::vector<string> > cells = std::vector<std::vector<string> >();

    parser.readNextRow(is);

    int columnCount = parser.size();

    for (int i = 0; i < columnCount; i++)
    {
		columns.push_back(parser[i]);
		cells.push_back(std::vector<string>());
    }

    parser.readNextRow(is);

    while (parser.size() > 0)
    {
        int i = 0;
        for (; i < parser.size() && i < columnCount; i++)
			cells[i].push_back(parser[i]);
        for (; i < columnCount; i++)
			cells[i].push_back(string());

        parser.readNextRow(is);
    }

	managed_shared_memory* mem = SharedMemory::create();

	return mem->construct<DataSet>(boost::interprocess::unique_instance)(mem, &columns, &cells);
}
