program example

  use mqc_gaussian

  implicit none
  character(len=:),allocatable::fileName
  type(mqc_gaussian_unformatted_matrix_file)::fileInfo
  type(mqc_matrix)::tmpMatrixAlpha
  type(mqc_scf_integral)::mo_coeffs
  logical::found

  call mqc_get_command_argument(1,fileName)
  write(6,'(A14,1X,A)') 'Input file is:',fileName
  call fileInfo%load(fileName)

  call fileInfo%getArray('ALPHA MO COEFFICIENTS',tmpMatrixAlpha,foundOut=found)
  call fileInfo%getESTObj('mo coefficients',est_integral=mo_coeffs,foundObj=found)

  call fileInfo%create('new_matrix_file.mat')
!  call fileInfo%writeArray('ALPHA MO COEFFICIENTS',tmpMatrixAlpha)
  call fileInfo%writeESTObj('mo coefficients',est_integral=mo_coeffs)

end program example
