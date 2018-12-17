// demo.f
// Author: Matthew Chan
// Demo program that shows off Fli-O


def addLineNumbers(string filename)
{
      file f = fopen(filename);

      // Create a copy of the current file
      string copyName = concat('lined_', filename);
      copy(filename, copyName);

      file newFile = fopen(copyName);

      string line = readLine(f);

      // Keep track of which line we are on
      int lineNo = 0;
      string prefix;

      // Loop through all of the lines in file f
      for(; strcmp(line, '') != 0;;) {
              prefix = concat('[', intToStr(lineNo));  
              prefix = concat(prefix, '] ');

              // Write the lined version to the new file
              write(newFile, concat(prefix, line));
              line = readLine(f);
              lineNo = lineNo + 1;
      }

}

addLineNumbers('sample.txt');
