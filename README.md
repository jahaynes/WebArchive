# WebArchive
WebArchive (WARC) Library

A minimal implementation of the WARC data format.
(see http://bibnum.bnf.fr/WARC/WARC_ISO_28500_version1_latestdraft.pdf)

There are a number of omissions, especially when using this library to generate WARC files, e.g.:
* No effort was made to lead the file with a 'warcinfo' block

However, this will likely be able to parse whatever WARC file you happen to have lying around.
