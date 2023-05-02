## Module and functions

### module BinaryReelHeaderModule
-  function readBinHeaderToArray(filename)
-  function readWordBinaryHeaderFromArray(binaryh_in,word_in)
-  subroutine changeWordBinaryHeaderInArray(binaryh_in,word_in,val_in)
-  subroutine writeBinaryHeaderToFile(binaryh_in,filename)
-  function readWordBinaryHeaderFromFile(filename,word_in)
-  function readWordBinaryHeaderTwoBytesFromFile(filename,byte_in)
-  function readWordBinaryHeaderFourBytesFromFile(filename,byte_in)
-  subroutine changeWordBinaryHeaderInFile(filename,word_in,val_in)
-  subroutine printBinaryHeader(filename,file_out)

### module EBCDICReelHeaderModule

- function readEBCDICHeader(filename)
- subroutine writeEBCDICHeader(ebcdich_in,filename)
- subroutine printEBCDICHeader(filename,file_out)



### module ReadSegyToolsModule
- function EBCDICtoASCII(ebcdic_in)
- function ASCIIToEBCDIC(ascii_in)
- subroutine checkFileExists(file_in)
- subroutine checkOpenSegyFile(file_in,unit_number,flag)


### module TraceReelHeaderModule
- function readTraceHeaderToArray(filename,ntrace)
- function readWordTraceHeaderToArray(traceh_in,word_in)
- subroutine changeWordTraceHeaderInArray(traceh_in,word_in,val_in)
- subroutine writeTraceHeaderInFile(traceh_in,ntrace,filename)
- function readWordTraceHeaderInFile(filename,ntrace,word_in)
- subroutine changeWordTraceHeaderInFile(filename,ntrace,word_in,val_in)
- subroutine printTraceHeader(filename,ntrace,file_out)
 

### module TracesModule
- function readTrace(filename,ntrace,nsmin,nsmax)
- subroutine writeTrace(filename,trace,ntrace)
- function countTraces(filename,ns)
- function maxValTraces(filename,tmin,tmax,smin,smax)
