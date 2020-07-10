program q4

use mqc_gaussian

implicit none
character(len=:),allocatable::fileName
type(mqc_gaussian_unformatted_matrix_file)::fileInfo
type(mqc_scf_integral)::density,coreHam,gMatrix,Fock
type(mqc_twoERIs)::twoERIs
type(mqc_molecule_data)::moleculeInfo
type(mqc_scalar)::Vnn,Energy

call mqc_get_command_argument(1,fileName)
call fileInfo%load(fileName)

call fileInfo%getESTObj('scf density',est_integral=density)
call fileInfo%getESTObj('core hamiltonian',est_integral=coreHam)
call fileInfo%getMolData(moleculeInfo)
call fileInfo%get2ERIs('regular',twoERIs)

Vnn = moleculeInfo%getNucRep()
call moleculeInfo%print(6)
call Vnn%print(6,'Nuclear Repulsion Energy (au)')

gMatrix = contraction([twoERIs],density)
Fock = coreHam + gMatrix
Energy = 0.5*contraction(density,coreHam+Fock) + Vnn
call Energy%print(6,'HF Energy (au)')

end program q4
