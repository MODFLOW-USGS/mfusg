      module sparse
      !cdl (7/17/2013) sparse matrix module for managing the structure
      !of a matrix.  Module uses FORTRAN 2003 extensions to manage
      !the data structures in an object oriented fashion.

          type rowtype
              integer :: nnz
              integer :: maxnnz
              integer,allocatable,dimension(:) :: icolarray
          end type rowtype

          type, public :: sparsematrix
              integer :: nrow
              integer :: ncol
              integer :: nnz
              integer,allocatable,dimension(:) :: ia
              integer,allocatable,dimension(:) :: ja
              logical :: iaalloc = .false.
              logical :: jaalloc = .false.
              type(rowtype),allocatable,dimension(:) :: row
              contains
                procedure :: init => initialize
                procedure :: addconnection
                procedure :: filliaja
                procedure :: sort
                procedure :: destroy
          end type sparsematrix
          
          contains

          subroutine initialize(this,nrow,ncol,rowmaxnnz)
              !initialize the sparse matrix.  This subroutine
              !acts a method for a sparse matrix by initializing
              !the row data.  rowmaxnnz is a vector of the max
              !number of values for each row.
              implicit none
              class(sparsematrix), intent(inout) :: this
              integer,intent(in) :: nrow,ncol
              integer,intent(in),dimension(nrow) :: rowmaxnnz
              integer :: i
              this%nrow = nrow
              this%ncol = ncol
              this%nnz = 0
              allocate(this%row(nrow))
              do i=1,nrow
                  allocate(this%row(i)%icolarray(rowmaxnnz(i)))
                  this%row(i)%icolarray=0
                  this%row(i)%nnz=0
                  this%row(i)%maxnnz=rowmaxnnz(i)
              enddo
          end subroutine initialize

          subroutine filliaja(this)
              !allocate and fill the ia and ja members of the 
              !sparsematrix.  
              implicit none
              class(sparsematrix), intent(inout) :: this
              integer :: i,j,ipos
              !call this%countnnz()
              if (this%iaalloc) deallocate(this%ia)
              if (this%jaalloc) deallocate(this%ja)
              allocate(this%ia(this%nrow+1))
              allocate(this%ja(this%nnz))
              this%iaalloc = .true.
              this%jaalloc = .true.
              ipos=1
              this%ia(1)=ipos
              do i=1,this%nrow
                  do j=1,this%row(i)%nnz
                      this%ja(ipos)=this%row(i)%icolarray(j)
                      ipos=ipos+1
                  enddo
                  this%ia(i+1)=ipos
              enddo
          end subroutine filliaja

          subroutine addconnection(this,i,j,inodup)
              !add a connection to the sparsematrix.  if inodup
              !(for no duplicates) is 1, then j is added only
              !if it is unique.
              implicit none
              class(sparsematrix), intent(inout) :: this
              integer,intent(in) :: i,j,inodup
              integer,allocatable,dimension(:) :: itmp
              integer :: jj,iadded
              call insert(i, j, this%row(i), 1, iadded)
              this%nnz=this%nnz+iadded
          end subroutine addconnection

          subroutine insert(i, j, thisrow, inodup, iadded)
              !insert j into the icolarray for row i
              !inodup=1 means do not include duplicate connections
              !iadded is 1 if a new entry was added (meaning that 
              !nnz for the row was increased) iadded is 0 if duplicate 
              !and not added.  Used to track total number of connections
              implicit none
              integer,intent(in) :: i,j,inodup
              type(rowtype),intent(inout) :: thisrow
              integer,allocatable,dimension(:) :: iwk
              integer,intent(inout) :: iadded
              integer :: jj
              iadded=0
              if (thisrow%icolarray(1) == 0) then
                  thisrow%icolarray(1) = j
                  thisrow%nnz = thisrow%nnz + 1
                  iadded=1
                  return
              endif
              if (thisrow%nnz == thisrow%maxnnz) then
                  !increase size of the row
                  allocate(iwk(thisrow%nnz))
                  iwk=thisrow%icolarray
                  deallocate(thisrow%icolarray)
                  thisrow%maxnnz=thisrow%maxnnz*2
                  allocate(thisrow%icolarray(thisrow%maxnnz))
                  thisrow%icolarray(1:thisrow%nnz)=iwk(1:thisrow%nnz)
                  thisrow%icolarray(thisrow%nnz+1:thisrow%maxnnz)=0
              endif
              if(inodup==1) then
                  do jj=1,thisrow%nnz
                      if(thisrow%icolarray(jj)==j) return
                  enddo
              endif
              !add the connection to end
              thisrow%nnz=thisrow%nnz+1
              thisrow%icolarray(thisrow%nnz)=j
              iadded=1
          end subroutine insert

          subroutine sort(this)
              !sort the icolarray for each row, but do not include
              !the diagonal position in the sort so that it stays in front
              implicit none
              class(sparsematrix), intent(inout) :: this
              integer :: i,nval
              do i=1,this%nrow
                  nval=this%row(i)%nnz
                  call sortintarray(nval-1,
     1                this%row(i)%icolarray(2:nval)) 
              enddo
          end subroutine sort

          subroutine sortintarray(nval,iarray)
              !simple subroutine for sorting an array
              !in place.  It is not the fastest sort function
              !but should suffice for relatively short nodelists.
              implicit none
              integer,intent(in) :: nval
              integer,intent(inout),dimension(nval) :: iarray
              integer :: i,j,itemp
              do i=1,nval-1
                  do j=i+1,nval
                      if(iarray(i)>iarray(j)) then
                          itemp=iarray(j)
                          iarray(j)=iarray(i)
                          iarray(i)=itemp
                      endif
                  enddo
              enddo
          end subroutine sortintarray

          subroutine destroy(this)
              implicit none
              class(sparsematrix), intent(inout) :: this
              if(this%iaalloc) then
                  deallocate(this%ia)
                  this%iaalloc=.false.
              endif
              if(this%jaalloc) then
                  deallocate(this%ja)
                  this%jaalloc=.false.
              endif
              deallocate(this%row)
          end subroutine destroy

      end module sparse

