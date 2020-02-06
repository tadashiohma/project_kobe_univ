program main
use omp_lib
implicit none

integer i,j,k,ix,iy,iz,mpij,myrank,nprocs
integer, parameter :: nxmax=96,nymax=96,nzmax=96,nkmax=11851
real*8 ::x,y,z,dx,dy,dz,dxinv,dyinv,dzinv,y1,stime,etime
real*8, allocatable ::f(:,:,:),g(:,:,:)
integer :: num_thread

allocate(f(0:nxmax+1,0:nymax+1,0:nzmax+1),g(0:nxmax+1,0:nymax+1,0:nzmax+1))

do iz=0,nzmax+1
  do iy=0,nymax+1
    do ix=0,nxmax+1
      dx=0.1d0
      dy=0.11d0
      dz=0.12d0
      x=ix*dx
      y=iy*dy
      z=iz*dz
      f(ix,iy,iz)=x*x + y*y + z*z
      g(ix,iy,iz)=0.0d0
    end do
  end do
end do


  dx=0.1d0
  dy=0.11d0
  dz=0.12d0
  dxinv=1.0d0/dx/dx
  dyinv=1.0d0/dy/dy
  dzinv=1.0d0/dz/dz

!$omp parallel default(firstprivate) private(k,ix,iy,iz) shared(f, g, stime, etime)
!$omp single
  write(6,*) "# Number of Threads:", omp_get_num_threads()
!$omp end single
!$omp single
  stime=omp_get_wtime()
!$omp end single

  do k=1,nkmax
    !$omp do collapse(2) schedule(static)  
    do iz=1,nzmax
      do iy=1,nymax
        do ix=1,nxmax
          g(ix,iy,iz)=g(ix,iy,iz) &
          +(f(ix+1,iy,iz)-2.0d0*f(ix,iy,iz)+f(ix-1,iy,iz))*dxinv &
          +(f(ix,iy+1,iz)-2.0d0*f(ix,iy,iz)+f(ix,iy-1,iz))*dyinv &
          +(f(ix,iy,iz+1)-2.0d0*f(ix,iy,iz)+f(ix,iy,iz-1))*dzinv
        end do
      end do
    end do
    !$omp end do
  end do

  !$omp barrier
  
  !$omp single
  etime=omp_get_wtime()
  !$omp end single


!$omp end parallel


write(6,*) sum(g), "# Checksum"
write(6,*) (etime-stime), "# Total Time [sec]"
write(6,*) (etime-stime) / nkmax * 1.0E3, "# Single Time [msec]"
write(6,*) 14.0d0*nxmax*nymax*nzmax*nkmax/(etime-stime)/1.0E9, "# Performance [GFlops]"

stop
end
