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


module WordDefinitionModule

  integer(kind=4), parameter::EBCDIC_HEADER_SIZE=3200,&
       BINARY_HEADER_SIZE=400,&
       TRACE_HEADER_SIZE=240
  
  !ebcdic_reel=========================================================

  character(len=1), dimension(EBCDIC_HEADER_SIZE)::ebcdich

  integer(kind=2), parameter, dimension(256)::EBCDIC_TABLE = [&
       000,001,002,003,156,009,134,127,151,141,142,011,012,013,014,015,&
       016,017,018,019,157,133,008,135,024,025,146,143,028,029,030,031,&
       128,129,130,131,132,010,023,027,136,137,138,139,140,005,006,007,&
       144,145,022,147,148,149,150,004,152,153,154,155,020,021,158,026,&
       032,160,161,162,163,164,165,166,167,168,213,046,060,040,043,124,&
       038,169,170,171,172,173,174,175,176,177,033,036,042,041,059,094,&
       045,047,178,179,180,181,182,183,184,185,229,044,037,095,062,063,&
       186,187,188,189,190,191,192,193,194,096,058,035,064,039,061,034,&
       195,097,098,099,100,101,102,103,104,105,196,197,198,199,200,201,&
       202,106,107,108,109,110,111,112,113,114,203,204,205,206,207,208,&
       209,126,115,116,117,118,119,120,121,122,210,211,212,091,214,215,&
       216,217,218,219,220,221,222,223,224,225,226,227,228,093,230,231,&
       123,065,066,067,068,069,070,071,072,073,232,233,234,235,236,237,&
       125,074,075,076,077,078,079,080,081,082,238,239,240,241,242,243,&
       092,159,083,084,085,086,087,088,089,090,244,245,246,247,248,249,&
       048,049,050,051,052,053,054,055,056,057,250,251,252,253,254,255] 


  
  !binary_reel words===================================================

  integer(kind=4), dimension(BINARY_HEADER_SIZE)::binaryh
   
  integer(kind=4), dimension(27), parameter::BINARY_WORD_BYTE_SIZE=&
       (/4,4,4,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2/)
  
  character(len=10), dimension(27), parameter::BINARY_WORD_NAME=&
       (/ character(len=10)::&
       'jid',&
       'ln',&
       'rn',&
       'ntr',&
       'natr',&
       'dt',&
       'dto',&
       'ns',&
       'nso',&
       'dsfc',&
       'cdpf',&
       'trsc',&
       'vsc',&
       'sfst',&
       'sfre',&
       'slen',&
       'stc',&
       'trns',&
       'strtls',&
       'strte',&
       'ttc',&
       'cdt',&
       'bgr',&
       'arm',&
       'msyst',&
       'isp',&
       'vpc'/)

  
  !trace_reel words=====================================================
  
  integer(kind=4), dimension(TRACE_HEADER_SIZE)::traceh
  
  integer(kind=4), dimension(71), parameter::TRACE_WORD_BYTE_SIZE=&
       (/4,4,4,4,4,4,4,&
       2,2,2,2,&
       4,4,4,4,4,4,4,4,&
       2,2,&
       4,4,4,4,&
       2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,&
       2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,&
       2,2,2,2,2,2 /)

  character(len=10), dimension(71), parameter::TRACE_WORD_NAME=&
       (/ character(len=10)::&
       'trnl',&
       'trnr',&
       'ofrn',&
       'tnofr',&
       'espn',&
       'cdpen',&
       'tnwcdp',&
       'dsr',&
       'rge',&
       'ses',&
       'sdbs',&
       'derg',&
       'des',&
       'wds',&
       'wdrg',&
       'xsc',&
       'ysc',&
       'xrc',&
       'yrc',&
       'tic',&
       'nvst',&
       'nhst',&
       'duse',&
       'sed',&
       'scoord',&
       'cunit',&
       'wvel',&
       'swvel',&
       'uts',&
       'utrg',&
       'ssc',&
       'rgsc',&
       'tsa',&
       'ltht',&
       'ltts',&
       'ltsr',&
       'smt',&
       'emt',&
       'ns',&
       'dt',&
       'fitc',&
       'igc',&
       'iegd',&
       'corr',&
       'sfs',&
       'sfe',&
       'slen',&
       'stc',&
       'stls',&
       'stle',&
       'ttc',&
       'aff',&
       'afs',&
       'nff',&
       'nfs',&
       'lcf',&
       'hcf',&
       'lcs',&
       'hcs',&
       'ydr',&
       'doy',&
       'hod',&
       'moh',&
       'som',&
       'tbas',&
       'trwf',&
       'ggnrs',&
       'ggnft',&
       'ggnlt',&
       'gs',&
       'oat'/)

end module WordDefinitionModule
