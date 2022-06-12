! Author: Simon Mason
Module data_text
!
! Contains declations and definitions of:
!    label sizes
!    alphabets
!    various language constants and identifiers
!
! Declarations
!
! Implicit declarations
   Implicit None
!
! Intrinsic functions
   Intrinsic Achar
!
! Parameters
!
! Integer parameters
   Integer, Parameter, Public ::      llab3 = 2**3       ! - length of very short label text -
   Integer, Parameter, Public ::      llab4 = 2**4       ! - length of short label text -
   Integer, Parameter, Public ::      llab5 = 2**5       ! - length of label text -
   Integer, Parameter, Public ::      llab6 = 2**6       ! - length of long label text -
   Integer, Parameter, Public ::      nlang = 5          ! - number of languages -
   Integer, Parameter, Public ::       nlet = 26         ! - number of letters -
   Integer, Parameter, Public ::      nalet = 27         ! - number of accented letters -
   Integer, Parameter, Public ::      ntlet = nlet+nalet ! - total number of letters -
   Integer, Parameter, Public ::    i_latin = 1          ! - Latin alphabet -
#if GFORTRAN == 1
   Integer, Parameter, Public :: i_cyrillic = 4          ! - Cyrillic alphabet -
#else
   Integer, Parameter, Public :: i_cyrillic = 3          ! - Cyrillic alphabet -
#endif
!
   Integer, Parameter, Private :: lla = 3 ! - length of language abbreviation -
!
   Integer, Dimension(nlang), Parameter, Public :: ialphabet = & ! - languages -
      [i_latin, &
       i_latin, &
       i_latin, &
       i_latin, &
       i_cyrillic]
!
! Character parameters
#if GUI == 1
   Character(Len=*), Parameter, Public :: c_akey = '&' ! - accelerator key -
#else
   Character(Len=*), Parameter, Public :: c_akey = ' ' ! - accelerator key -
#endif
   Character(Len=*), Parameter, Public :: c_letters_u = & ! - upper-case letters -
      'ABCDEFGHIJKLMNOPQRSTUVWXYZÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÑÒÓÔÕÖØÙÚÛÜ'
   Character(Len=*), Parameter, Public :: c_letters_l = & ! - lower-case letters -
      'abcdefghijklmnopqrstuvwxyzàáâãäåæçèéêëìíîïñòóôõöøùúûü'
!
   Character(Len=*), Parameter, Public :: c_cyrillic_u = & ! - upper-case letters -
      'АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ'
   Character(Len=*), Parameter, Public :: c_cyrillic_l = & ! - lower-case letters -
      'абвгдеёжзийклмнопрстуфхцчшщъыьэюя'
!
   Character(Len=1), Parameter, Public :: c_bckspace = Achar(8) ! - backspace -
   Character(Len=1), Parameter, Public ::      c_tab = Achar(9) ! - tab -
!
   Character(Len=lla), Dimension(nlang), Parameter, Public :: ca_lang_a = & ! - language abbreviations -
      ['eng', &
       'esp', &
       'fra', &
       'por', &
       'rus']
   Character(Len=*, Kind=i_cyrillic), Dimension(nlang), Parameter, Public :: ca_lang = & ! - languages -
      [i_cyrillic_'English         ', &
       i_cyrillic_'Español        ', &
       i_cyrillic_'Français       ', &
       i_cyrillic_'Português      ', &
       i_cyrillic_'Русский  ']
#if GUI == 1
   Character(Len=*, Kind=i_cyrillic), Dimension(nlang), Parameter, Public :: ca_langlist = & ! - language list -
      [i_cyrillic_'&English         ', &
       i_cyrillic_'E&spañol        ', &
       i_cyrillic_'&Français       ', &
       i_cyrillic_'&Português      ', &
       i_cyrillic_'&Русский  ']
#endif
!
! Scalars
!
! Integer scalars
   Integer, Public :: ilang = 1 ! - current language -
!
End Module data_text
