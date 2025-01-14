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

module ReadSegyToolsModule

  use WordDefinitionModule, only:&
       EBCDIC_TABLE
  

  
contains

  !==========================================================================

  function EBCDICtoASCII(ebcdic_in)

    !in
    character(len=1)::ebcdic_in

    !out
    character(len=1)::EBCDICtoASCII

    !local
    character(len=1)::aux
    integer (kind=4)::i
  
    EBCDICtoASCII=char(EBCDIC_TABLE(ichar(ebcdic_in)+1))
  
    return
    
  end function EBCDICtoASCII

  !==========================================================================

  function ASCIIToEBCDIC(ascii_in)

    !in
    character(len=1)::ascii_in

    !out
    character(len=1)::ASCIIToEBCDIC

    !local
    integer(kind=4)::i
    integer(kind=4), dimension(1)::pos
    
    pos=pack([(i,i=1,256)],EBCDIC_TABLE==(ichar(ascii_in)))
    ASCIIToEBCDIC=char(pos(1)-1)
        
    
    return
    
  end function ASCIIToEBCDIC

  !==========================================================================

  subroutine checkFileExists(file_in)
    
    character(len=*):: file_in
    ! variables de la info del archivo
    integer, dimension(13) :: buff
    integer:: status

    call stat(file_in, buff, status)
    if (status /= 0 )then
       write(*,*)"The file do not exists or is not readable."
       stop
    end if
    
  end subroutine checkFileExists

  !==========================================================================
    
  subroutine checkOpenSegyFile(file_in,unit_number,flag)
    
    character(len=*):: file_in
    integer(kind=4)::unit_number,flag
    
    !check if filneame is connected to any unit
    !if unit_name=-1 then file_in is not conected and then is opened,
    !otherwise is connected to unit_number
    inquire(file=file_in, number=unit_number)

    
    !if unit_number=-1 then open the file in a new unit
    !if a new unit is open, flag=-1
    !newunit option is used
    if(unit_number==-1)then
       flag=unit_number
       open(newunit=unit_number, &
            file=file_in,&
            form = 'unformatted', &
            access = 'stream',&
            convert = 'native')
    end if
    
  end subroutine checkOpenSegyFile
  
  
end module ReadSegyToolsModule
