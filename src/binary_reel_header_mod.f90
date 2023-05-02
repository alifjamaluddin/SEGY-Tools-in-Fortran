! This program is free software: you can redistribute it and/or modify it under
! the terms of the GNU General Public License as published by the Free Software
! Foundation, either version 3 of the License, or (at your option) any later
! version.
! 
! This program is distributed in the hope that it will be useful, but WITHOUT
! ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
! FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License along with
! this program. If not, see <http://www.gnu.org/licenses/>.
!
!----------------------------------------------------------------------------
! Created By  : Daniel O. Perez
! Created Date: August of 2016
! email: perez.daniel.omar@gmail.com
! ---------------------------------------------------------------------------


module BinaryReelHeaderModule

  use WordDefinitionModule, only:BINARY_WORD_NAME,&
       BINARY_WORD_BYTE_SIZE,&
       EBCDIC_HEADER_SIZE,&
       BINARY_HEADER_SIZE
  use ReadSegyToolsModule, only:checkOpenSegyFile
  implicit none

  interface readWordBinaryHeader
     module procedure readWordBinaryHeaderFromArray,&
          readWordBinaryHeaderFromFile,&
          readWordBinaryHeaderTwoBytesFromFile,&
          readWordBinaryHeaderFourBytesFromFile
  end interface readWordBinaryHeader

  interface changeWordBinaryHeader
     module procedure changeWordBinaryHeaderInArray, changeWordBinaryHeaderInFile
  end interface changeWordBinaryHeader

contains

  !read binary header to array from file  
  function readBinHeaderToArray(filename)

    !in 
    character(len=*), intent(in):: filename

    !out
    integer(kind=4), dimension(BINARY_HEADER_SIZE)::readBinHeaderToArray

    !local
    integer(kind=4)::i,j,unit_number,flag,sum_bsize
    integer(kind=4), dimension(1)::word_pos
    integer(kind=2)::word2b


    !check if filneame is connected to any unit
    !if flag==-1, a new unit was openend and must be closed at program end
    call checkOpenSegyFile(filename,unit_number,flag)

    sum_bsize=0
    readBinHeaderToArray=0

    do i=1,size(BINARY_WORD_NAME)

       !read word_in value as int*2 or int*4 according binaryw_size
       !the ouptut is int*4 always
       sum_bsize=sum(BINARY_WORD_BYTE_SIZE(1:i-1))
       select case(BINARY_WORD_BYTE_SIZE(i))
       case(2)
          read(unit=unit_number,pos=EBCDIC_HEADER_SIZE+sum_bsize+1)word2b
          readBinHeaderToArray(i)=int(word2b,kind=4)
       case(4)
          read(unit=unit_number,pos=EBCDIC_HEADER_SIZE+sum_bsize+1)&
               readBinHeaderToArray(i)
       end select

    end do

    if(flag==-1)close(unit_number)

    return

  end function readBinHeaderToArray

  !=====================================================================

  !read binary header word from array
  function readWordBinaryHeaderFromArray(binaryh_in,word_in)

    !in
    integer(kind=4), intent(in), dimension(:)::binaryh_in
    character(len=*), intent(in)::word_in

    !out
    integer(kind=4)::readWordBinaryHeaderFromArray

    !local
    integer(kind=4)::i
    integer(kind=4), dimension(1)::word_pos


    !check if word exists
    if(any(BINARY_WORD_NAME==word_in)) then

       !read word_in value as int*2 or int*4 according binaryw_size
       !the ouptut is int*4 always

       word_pos=pack([(i,i=1,size(BINARY_WORD_NAME))],BINARY_WORD_NAME==word_in)
       readWordBinaryHeaderFromArray=binaryh_in(word_pos(1))

       return

    else

       write(*,*)"Error: no such binary word:",word_in
       stop

    end if


  end function readWordBinaryHeaderFromArray


  !=====================================================================

  !change binary header word in array
  subroutine changeWordBinaryHeaderInArray(binaryh_in,word_in,val_in)

    !in
    integer(kind=4), intent(inout), dimension(:)::binaryh_in
    integer(kind=4), intent(in):: val_in
    character(len=*), intent(in)::word_in

    !local
    integer(kind=4)::i
    integer(kind=4), dimension(1)::word_pos


    !check if word exists
    if(any(BINARY_WORD_NAME==word_in)) then

       !read word_in value as int*2 or int*4 according binaryw_size
       !the ouptut is int*4 always

       word_pos=pack([(i,i=1,size(BINARY_WORD_NAME))],BINARY_WORD_NAME==word_in)
       binaryh_in(word_pos(1))=val_in

       return

    else

       write(*,*)"Error: no such binary word:",word_in
       stop

    end if

  end subroutine changeWordBinaryHeaderInArray

  !=====================================================================

  !write binary header from array to file
  subroutine writeBinaryHeaderToFile(binaryh_in,filename)

    !in 
    character(len=*), intent(in):: filename
    integer(kind=4), intent(in), dimension(:)::binaryh_in

    !local
    integer(kind=4)::i,unit_number,flag,sum_bsize

    !check if filneame is connected to any unit
    !if flag==-1, a new unit was openend and must be closed at program end
    call checkOpenSegyFile(filename,unit_number,flag)


    do i=1,size(BINARY_WORD_NAME)

       !read word_in value as int*2 or int*4 according binaryw_size
       !the ouptut is int*4 always
       sum_bsize=sum(BINARY_WORD_BYTE_SIZE(1:i-1))

       select case(BINARY_WORD_BYTE_SIZE(i))
       case(2)
          write(unit=unit_number,pos=EBCDIC_HEADER_SIZE+sum_bsize+1)&
               int(binaryh_in(i),kind=2)
       case(4)
          write(unit=unit_number,pos=EBCDIC_HEADER_SIZE+sum_bsize+1)&
               binaryh_in(i)
       end select

    end do

    !zeros to complete the header 
    do i=sum(BINARY_WORD_BYTE_SIZE),BINARY_HEADER_SIZE-1
       write(unit=unit_number,pos=EBCDIC_HEADER_SIZE+i+1)&
            int(0,kind=1)
    end do


    if(flag==-1)close(unit_number)       


  end subroutine writeBinaryHeaderToFile


  !=====================================================================

  !read binary header word from file
  function readWordBinaryHeaderFromFile(filename,word_in)

    !in 
    character(len=*), intent(in):: filename,word_in

    !out
    integer(kind=4)::readWordBinaryHeaderFromFile

    !local
    integer(kind=4)::i,unit_number,flag,sum_bsize
    integer(kind=4), dimension(1)::word_pos
    integer(kind=2)::word2b

    sum_bsize=0

    !check if word exists
    if(any(BINARY_WORD_NAME==word_in)) then

       !check if filneame is connected to any unit
       !if flag==-1, a new unit was openend and must be closed at program end
       call checkOpenSegyFile(filename,unit_number,flag)

       !check word_in position inside binaryw_name
       word_pos=pack([(i,i=1,size(BINARY_WORD_NAME))],BINARY_WORD_NAME==word_in)

       !read word_in value as int*2 or int*4 according binaryw_size
       !the ouptut is int*4 always
       sum_bsize=sum(BINARY_WORD_BYTE_SIZE(1:word_pos(1)-1))
       select case(BINARY_WORD_BYTE_SIZE(word_pos(1)))
       case(2)
          read(unit=unit_number,pos=EBCDIC_HEADER_SIZE+sum_bsize+1)word2b
          readWordBinaryHeaderFromFile=int(word2b,kind=4)
       case(4)
          read(unit=unit_number,pos=EBCDIC_HEADER_SIZE+sum_bsize+1)readWordBinaryHeaderFromFile
       end select

       if(flag==-1)close(unit_number)       

       return

    else

       write(*,*)"Error: no such binary word:",word_in
       stop

    end if


  end function readWordBinaryHeaderFromFile

    !=====================================================================

  !read binary header word from file, at a given byte
  function readWordBinaryHeaderTwoBytesFromFile(filename,byte_in)

    !in 
    character(len=*), intent(in):: filename
    integer(kind=2), intent(in)::byte_in

    !out
    integer(kind=2)::readWordBinaryHeaderTwoBytesFromFile

    !local
    integer(kind=4)::i,unit_number,flag,sum_bsize
    integer(kind=4), dimension(1)::word_pos
    
    if(byte_in+2.gt.BINARY_HEADER_SIZE)then
       write(*,*)'Byte out of boundaries'
       stop
    end if

    !check if filneame is connected to any unit
    !if flag==-1, a new unit was openend and must be closed at program end
    call checkOpenSegyFile(filename,unit_number,flag)

    read(unit=unit_number,pos=EBCDIC_HEADER_SIZE+byte_in)readWordBinaryHeaderTwoBytesFromFile
    
    if(flag==-1)close(unit_number)   

    return

  end function readWordBinaryHeaderTwoBytesFromFile

  !=====================================================================

  !read binary header word from file, at a given byte
  function readWordBinaryHeaderFourBytesFromFile(filename,byte_in)

    !in 
    character(len=*), intent(in):: filename
    integer(kind=4), intent(in)::byte_in

    !out
    integer(kind=4)::readWordBinaryHeaderFourBytesFromFile

    !local
    integer(kind=4)::i,unit_number,flag,sum_bsize
    integer(kind=4), dimension(1)::word_pos
    
    if(byte_in+4.gt.BINARY_HEADER_SIZE)then
       write(*,*)'Byte out of boundaries'
       stop
    end if

    !check if filneame is connected to any unit
    !if flag==-1, a new unit was openend and must be closed at program end
    call checkOpenSegyFile(filename,unit_number,flag)

    read(unit=unit_number,pos=EBCDIC_HEADER_SIZE+byte_in)readWordBinaryHeaderFourBytesFromFile
    
    if(flag==-1)close(unit_number)   

    return

  end function readWordBinaryHeaderFourBytesFromFile

  !=====================================================================

  !change binary header word in file
  subroutine changeWordBinaryHeaderInFile(filename,word_in,val_in)

    !in 
    character(len=*), intent(in):: filename,word_in
    integer(kind=4), intent(in):: val_in

    !local
    integer(kind=4)::i,unit_number,flag,sum_bsize
    integer(kind=4), dimension(1)::word_pos

    !check if word exists
    if(any(BINARY_WORD_NAME==word_in)) then

       !check if filneame is connected to any unit
       !if flag==-1, a new unit was openend and must be closed at program end
       call checkOpenSegyFile(filename,unit_number,flag)

       word_pos=pack([(i,i=1,size(BINARY_WORD_NAME))],BINARY_WORD_NAME==word_in)

       !read word_in value as int*2 or int*4 according binaryw_size
       !the ouptut is int*4 always
       sum_bsize=sum(BINARY_WORD_BYTE_SIZE(1:word_pos(1)-1))
       select case(BINARY_WORD_BYTE_SIZE(word_pos(1)))
       case(2)
          write(unit=unit_number,pos=EBCDIC_HEADER_SIZE+sum_bsize+1)int(val_in,2)
       case(4)
          write(unit=unit_number,pos=EBCDIC_HEADER_SIZE+sum_bsize+1)val_in
       end select


       if(flag==-1)close(unit_number) 

    else

       write(*,*)"Error: no such binary word:",word_in
       stop

    end if

  end subroutine changeWordBinaryHeaderInFile

  !=====================================================================

  !print binary header from file, to another file or screen
  subroutine printBinaryHeader(filename,file_out)

    !in
    character(len=*), intent(in)::filename,file_out

    !local
    integer(kind=4)::unit_number,i
    character(len=100), dimension(27)::def_array
    !check output unit
    if(file_out=='screen')then
       unit_number=6
    else
       open(newunit=unit_number,file=file_out,action='write')
    end if


    def_array=(/character(len=100)::"Job identification number",&
         "Line number",&
         "Reel number",&
         "Number of data traces per record",&
         "Number of auxiliary traces per record",&
         "Sample interval of this reel's data in microseconds",&
         "Sample interval of original field recording in microseconds",&
         "Number of samples per trace for this reel's data",&
         "Number of samples per trace in original field recording",&
         "Data sample format code",&
         "CDP fold",&
         "Trace sorting code",&
         "Vertical sum code",&
         "Sweep frequency at start in Hertz",&
         "Sweep frequency at end in Hertz",&
         "Sweep length in milliseconds",&
         "Sweep type code",&
         "Trace number of sweep channel",&
         "Sweep trace taper length at start in milliseconds",&
         "Sweep trace taper length at end in milliseconds",&
         "Taper type code",&
         "Correlated data traces",&
         "Binary gain recovered",&
         "Amplitude recovery method code",&
         "Measurement system",&
         "Impulse signal polarity",&
         "Vibratory polarity code"/)

    do i=1,size(def_array)
       write(unit_number,100)readWordBinaryHeaderFromFile(filename,BINARY_WORD_NAME(i)),def_array(i)
    end do

100 format(I10,2X,A65,1X)

  end subroutine  printBinaryHeader


  !=====================================================================


end module BinaryReelHeaderModule


