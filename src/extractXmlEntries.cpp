#include <Rcpp.h>
#include <fstream>
#include <sys/stat.h>
#include <string.h>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

struct Tag {
	const char* start;
	const char* stop;
	const char* start_end;
	const char* stop_end;
	const char* p;
	bool inside;
	bool is_on_start_tag;
	bool is_on_stop_tag;

	Tag(const char* start, const char* stop) {
		this->start = start;
		this->stop = stop;
		this->start_end = start + strlen(start) - 1;
		this->stop_end = stop + strlen(stop) - 1;
		this->reset();
	}

	void reset() {
		this->p = this->start;
		this->inside = false;
		this->is_on_start_tag = false;
		this->is_on_stop_tag = false;
	}

	bool isInside() {
		return this->inside;
	}

	bool isOnStartTag() {
		return this->is_on_start_tag;
	}

	bool isOnStopTag() {
		return this->is_on_stop_tag;
	}

	void advance(char c) {

		this->is_on_start_tag = false;
		this->is_on_stop_tag = false;

		// Look for entry start tag
		if ( ! this->inside) {
			if (c == *this->p) {
				if (this->p == this->start_end) {
					this->is_on_start_tag = true;
					this->inside = true;
					this->p = this->stop;
				}
				else
					++this->p;
			}
			else
				this->p = this->start;
		}

		// Look for entry end tag
		else {
			if (c == *this->p) {
				if (this->p == this->stop_end) {
					this->is_on_stop_tag = true;
					this->inside = false;
					this->p = this->start;
				}
				else
					++this->p;
			}
			else
				this->p = this->stop;
		}
	}
};

// ' Extract entries from HMDB XML database file.
// '
// ' The input XML file is read entirely and each entry (e.g.: '<metabolite>'
// element) is extracted and written in a separate file inside the destination
// folder.
// '
// ' @param xmlFile Path of the HMDB XML database file.
// ' @param extractDir Path of the folder where to extract entries.
// ' @return A character vector containing the paths to the extracted entries.
// ' Names are set to the entry accessions.
// '
// ' @export
// [[Rcpp::export]]
Rcpp::StringVector extractXmlEntries(const std::string& xmlFile,
                                     const std::string& extractDir) {
	Tag entry_tag("<metabolite>", "</metabolite>");
	Tag id_tag("<accession", "</accession>");

	Rcpp::StringVector entryFiles;

	// Check destination folder exists
	struct stat info;
	if (stat(extractDir.c_str(), &info) != 0 || ! (info.st_mode & S_IFDIR))
		Rcpp::stop("Destination folder \"%s\" does not exist.", extractDir.c_str());

	// Open XML file
	std::ifstream inf(xmlFile.c_str());

	// Check XML file exists
	if ( ! inf.good())
		Rcpp::stop("XML file does not exist.");

	// Read input file character by character
	int file_index = 0;
	std::string entry_filename;
	std::string entry_id;
	bool entry_id_complete = false;
	std::vector<std::string> entry_ids;
	std::ofstream *outf = NULL;
	int c;
	while ((c = inf.get()) != EOF) {

		entry_tag.advance(c);
		if (entry_tag.isOnStartTag()) {
			// Start writing to file
			std::ostringstream filename;
			filename << extractDir << "/entry_" << ++file_index << ".xml";
			entry_filename = filename.str();
			outf = new std::ofstream(entry_filename.c_str());
			*outf << entry_tag.start;
			id_tag.reset();
			entry_id.erase();
			entry_id_complete = false;
		}
		else if (entry_tag.isOnStopTag()) {
			// End writing to file
			outf->put(c);
			outf->close();
			outf = NULL;
			entry_tag.reset();
			if ( ! entry_id_complete)
				Rcpp::stop("Extracted HMDB entry has no accession number.");
			entryFiles.push_back(entry_filename);
			entry_ids.push_back(entry_id);
		}
		else if (entry_tag.isInside()) {
			// Write to file
			outf->put(c);
			// Look for ID tag
			if ( ! entry_id_complete) {
				id_tag.advance(c);
				if (id_tag.isInside() && ! id_tag.isOnStartTag())
					entry_id += c;
				if (id_tag.isOnStopTag()) {
					entry_id_complete = true;
					// Search for first `>`
					size_t sup_index = entry_id.find('>');
					// Search backward for first `<`
					size_t inf_index = entry_id.rfind('<');
					// Extract accession
					entry_id = entry_id.substr(sup_index + 1, inf_index - sup_index - 1);
				}
			}
		}
	}

	// Close XML file
	inf.close();

	entryFiles.attr("names") = entry_ids;

	return entryFiles;
}
