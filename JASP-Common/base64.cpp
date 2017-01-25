//
// Copyright (C) 2013-2017 University of Amsterdam
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

#include "base64.h"

#include "base64/cencode.h"
#include "base64/cdecode.h"

using namespace std;

const char *Base64::FileNameEncoding = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";
const char *Base64::RVarEncoding = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789._";

string Base64::encode(const string &prefix, const string &in, const char *encoding)
{
	base64_encodestate state;
	base64_init_encodestate(&state, encoding);

	int triplets = in.length() / 3;
	if (in.length() % 3)
		triplets++;

	char buffer[prefix.size() + 4 * triplets];

	size_t count;
	for (count = 0; count < prefix.length(); count++)
		buffer[count] = prefix.c_str()[count];

	count += base64_encode_block(in.c_str(), in.length(), &buffer[count], &state);
	count += base64_encode_blockend(&buffer[count], &state);

	return string(buffer, count);
}

string Base64::decode(const string &prefix, const string &in, const char *encoding)
{
	base64_decodestate state;
	base64_init_decodestate(&state, encoding);

	int length = in.length() - prefix.size();
	int quads = length / 4;
	if (length % 4)
		quads++;

	char outBuf[3 * quads];
	const char *inBuf = &in.c_str()[prefix.size()];

	int count = base64_decode_block(inBuf, length, outBuf, &state);

	return string(outBuf, count);
}
