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


module EBCDICReelHeaderModule

  use WordDefinitionModule, only:&
       ebcdich,&
       EBCDIC_HEADER_SIZE
  use ReadSegyToolsModule, only:&
       EBCDICtoASCII,&
       ASCIIToEBCDIC,&
       checkOpenSegyFile

  implicit none
  
contains
  

  !======================================================================

  function readEBCDICHeader(filename)

    !in 
    character(len=*), intent(in):: filename
    
    !out
    character(len=1), dimension(EBCDIC_HEADER_SIZE) :: readEBCDICHeader

    !local
    integer(kind=4)::unit_number,flag,i
    character(len=1)::ebcdic_ch
    
    !check if filneame is connected to any unit
    !if flag==-1, a new unit was openend and must be closed at program end
    call checkOpenSegyFile(filename,unit_number,flag)

    do i=1,EBCDIC_HEADER_SIZE
       read(unit=unit_number,pos=i)ebcdic_ch
       readEBCDICHeader(i)=EBCDICtoASCII(ebcdic_ch)
    end do
       
    if(flag==-1)close(unit_number)

    return
    
  end function readEBCDICHeader


  !======================================================================
  

  subroutine writeEBCDICHeader(ebcdich_in,filename)

    !in 
    character(len=*), intent(in):: filename
    character(len=1), intent(in), dimension(:)::ebcdich_in
   
    !local
    integer(kind=4)::unit_number,flag,i
    
    !check if filneame is connected to any unit
    !if flag==-1, a new unit was openend and must be closed at program end
    call checkOpenSegyFile(filename,unit_number,flag)

    do i=1,size(ebcdich_in)
       write(unit=unit_number,pos=i)ASCIIToEBCDIC(ebcdich_in(i))
    end do

    !empty spaces (yeah!!) to complete the header 
    do i=size(ebcdich_in),EBCDIC_HEADER_SIZE-1
       write(unit=unit_number,pos=size(ebcdich_in)+i+1)ASCIIToEBCDIC('')
    end do
    
    
    if(flag==-1)close(unit_number)

    return
    
  end subroutine writeEBCDICHeader
  
  !======================================================================

  subroutine printEBCDICHeader(filename,file_out)

    !in
    character(len=*), intent(in)::filename,file_out

    !local
    character(len=1), dimension(EBCDIC_HEADER_SIZE) :: ebcdich
    integer(kind=4)::unit_number,flag,i,j

    !check output unit
    if(file_out=='screen')then
       unit_number=6
       flag=0
    else
       open(newunit=unit_number,file=file_out,action='write')
       flag=-1
    end if

    !the file is opened and the ebcdic header readed
    !call ebcdic_reel_header_read(filename,ebcdich)

    ebcdich=readEBCDICHeader(filename)
    
    do i=1,40
       write(unit_number,*)(ebcdich(j+(i-1)*80),j=1,80)
    end do

    !close opened unit 
    if(flag==-1)close(unit_number)
    
  end subroutine printEBCDICHeader
  
end module EBCDICReelHeaderModule


  
