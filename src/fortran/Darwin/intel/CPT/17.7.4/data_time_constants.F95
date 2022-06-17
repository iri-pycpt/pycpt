! Author: Simon Mason
Module data_time_constants
!
! Contains declarations and definitions of:
!    calendar parameters
!    derived types for handling dates
!    time sequencing parameters
!
! Modules
   Use data_numbers, Only: rp
   Use data_text,    Only: nlang
!
! Declarations
!
! Implicit declarations
   Implicit None
!
! Parameters
!
! Integer parameters
   Integer, Parameter, Public :: lcmon = 3       ! - length of month abbreviations -
   Integer, Parameter, Public :: lfeb = 28       ! - length of non-leap year February -
   Integer, Parameter, Public :: lmon = 9        ! - length of month string -
   Integer, Parameter, Public :: mdm = 31        ! - maximum number of days per month -
   Integer, Parameter, Public :: mdy = 366       ! - maximum number of days per year -
   Integer, Parameter, Public :: msq = 10        ! - maximum sequence identifier -
   Integer, Parameter, Public :: ndw = 7         ! - number of days per week -
   Integer, Parameter, Public :: nmn = 12        ! - number of months -
   Integer, Parameter, Public :: nsq = 8         ! - number of sequences (excluding 0) -
   Integer, Parameter, Public :: nspd = 60*60*24 ! - number of seconds per day -
   Integer, Parameter, Public :: isq_yr = -3     ! - yearly sequencing -
   Integer, Parameter, Public :: isq_sn = -2     ! - seasonal sequencing (e.g., JFM, AMJ, etc; gaps are permitted) -
   Integer, Parameter, Public :: isq_so = -4     ! - seasonal overlapping monthly sequencing (e.g., JFM, FMA, etc) -
   Integer, Parameter, Public :: isq_mn = -1     ! - monthly sequencing -
   Integer, Parameter, Public :: isq_dy =  1     ! - daily sequencing -
!
! Real parameters
   Real(Kind=rp), Parameter, Public :: secpmin = 60.0_rp ! - number of seconds per minute -
   Real(Kind=rp), Parameter, Public :: minphr =  60.0_rp ! - number of minutes per hour -
   Real(Kind=rp), Parameter, Public :: hrpdy =   24.0_rp ! - number of hours per day -
!
! Character parameters
   Character(Len=*), Dimension(nlang), Parameter, Public :: cma = & ! - month abbeviations - 
      ['JFMAMJJASONDJFMAMJJASOND', &
       'EFMAMJJASONDEFMAMJJASOND', &
       'JFMAMJJASONDJFMAMJJASOND', &
       'JFMAMJJASONDJFMAMJJASOND', &
       'JFMAMJJASONDJFMAMJJASOND']
!
   Character(Len=*), Dimension(nmn), Parameter, Public :: cmon_l = & ! - months - 
      ['jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec']
!
   Character(Len=*), Dimension(nmn,nlang), Parameter, Public :: cmon = & ! - months - 
      Reshape(Source= &
      ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec', &
       'Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Set', 'Oct', 'Nov', 'Dic', &
       'Jan', 'Fév', 'Mar', 'Avr', 'Mai', 'Jun', 'Jul', 'Aoû', 'Sep', 'Oct', 'Nov', 'Déc', &
       'Jan', 'Fev', 'Mar', 'Abr', 'Mai', 'Jun', 'Jul', 'Ago', 'Set', 'Out', 'Nov', 'Dez', &
       'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'], &
      Shape=[nmn, nlang])
!
   Character(Len=*), Dimension(nmn,nlang), Parameter, Public :: umon = & ! - months - 
      Reshape(Source= &
      ['JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC', &
       'ENE', 'FEB', 'MAR', 'ABR', 'MAY', 'JUN', 'JUL', 'AGO', 'SET', 'OCT', 'NOV', 'DIC', &
       'JAN', 'FÉV', 'MAR', 'AVR', 'MAI', 'JUN', 'JUL', 'AOÛ', 'SEP', 'OCT', 'NOV', 'DÉC', &
       'JAN', 'FEV', 'MAR', 'ABR', 'MAI', 'JUN', 'JUL', 'AGO', 'SET', 'OUT', 'NOV', 'DEZ', &
       'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'], &
      Shape=[nmn, nlang])
!
   Integer, Dimension(-4:msq), Parameter, Public :: iuseq = & ! - used sequences -
      [ 2, 1, 2, 3, 4, 5, 4, 4, 6, 7, 4, 8, 4, 4, 9]
!
End Module data_time_constants
