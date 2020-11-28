	! Copyright 2005-2019 ECMWF. 中文注释: chinagod
	!
	! This software is licensed under the terms of the Apache Licence Version 2.0
	! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
	!
	! In applying this licence, ECMWF does not waive the privileges and immunities
	! granted to it by virtue of its status as an intergovernmental organisation
	! nor does it submit to any jurisdiction.
	!
	!
	!  Description: How to create and use and index to access messages from a file
	!               Creation of grib message, cloning one input message..
	!
	! 描述: 怎样从一个文件创建和使用index来访问消息
	!       创建grib消息，复制一个输入消息
	!
	program index
	use eccodes
	implicit none

	integer              :: iret
	integer,dimension(:),allocatable :: paramId,number
	integer                          :: paramIdSize,numberSize
	integer              :: i,j
	integer              :: idx,igrib,igribout,count,numberOfValues
	integer              :: outfile
	real (KIND=8),dimension(:), allocatable   :: values,result

	! create an index for the file 'eps' file using the keys number and paramId
	! use codes_index_create
	! 使用codes_index_createh函数，根据关键字number和paramId，创建文件'eps'的索引
	call codes_index_create(idx,'eps','paramId,number')

	! get the number of distinct values of paramId in the index
	! use codes_index_get_size
	! 使用codes_index_get_size函数，获得索引中paramID的值的数目
	call codes_index_get_size(idx,'paramId',paramIdSize)

	! allocate the array to contain the list of distinct paramId
	! 分配数组，用以存放paramId
	allocate(paramId(paramIdSize))

	! get the list of distinct paramId from the index
	! use codes_index_get
	!使用codes_index_get函数，根据索引读取paramId的值
	call codes_index_get(idx,'paramId',paramId)

	print*, 'grib contains ',paramIdSize, 'different parameters'

	! get the number of distinct values of number in the index
	! use codes_index_get_size
	!使用codes_index_get_size函数，从索引中获得number的值的数目
	call codes_index_get_size(idx,'number',numberSize)

	! allocate the array to contain the list of distinct numbers
	!分配数组，用以存放number
	allocate(number(numberSize))

	! get the list of distinct numbers from the index
	! use codes_index_get
	!使用codes_index_get函数，根据索引读取numbers的值
	call codes_index_get(idx,'number',number)

	print*, 'grib contains ',numberSize, 'different EPS members'

	count=0

	! select paramId 130 - Temperature
	! use codes_index_select
	!使用codes_index_select，选定paramID为130——对应变量为温度
	call codes_index_select(idx,'paramId','130')

	! loop over the different ensemble members
	!在不同集合成员直接循环
	do j=1,numberSize ! loop on number !number循环

		! select ensemble number=number(j)
		! use codes_index_select
		!使用codes_index_select，选定集合序号=number(j)
		call codes_index_select(idx,'number',number(j))

		! get one grib message for the above values of the index
		! use codes_new_from_index
		!使用codes_new_from_index，根据上述索引的值，得到一个grib的消息
		call codes_new_from_index(idx,igrib)

		! for the first filed allocate array for values and result
		! use codes_get_size
		!使用use codes_get_size函数获取数组大小，对第一个场，为values和results分配数组
		if ( j == 1 ) then
			call codes_get_size(igrib,'values',numberOfValues)
			allocate(values(numberOfValues), stat = iret)
			if (iret /= 0)  STOP 'Failed to allocate values'
			allocate(result(numberOfValues), stat = iret)
			if (iret /= 0)  STOP 'Failed to allocate result'

			! clone grib message
			! use codes_clone:
			!   codes_clone (gribid_src, gribid_dest [, status])
			!使用codes_clone (gribid_src, gribid_dest [, status])函数，复制grib消息
			call codes_clone( igrib, igribout)

		end if

		! get data values
		! use codes_get
		!使用use codes_get，获得data的值
		call codes_get(igrib,"values", values)

		count = count + 1

		result=result+values

		! release the grib message
		! use grib_release
		!使用grib_release，关闭grib消息
		call codes_release(igrib)

	end do ! loop on number

	! release the index
	! use codes_index_release
	!使用grib_release，关闭索引
	call codes_index_release(idx)

	print*,'We considered ',count,' members'

	result=result/count

	print*,'=============================================================================='
	print*, 'Stats for ensemble mean of T850'
	print*, 'Min: ', minval(result), 'Max: ', maxval(result), 'Avg: ', sum(result)/numberOfValues
	print*,'=============================================================================='

	! open output grib file eps_mean.grib
	! use codes_open_file:
	!   codes_open_file (ifile, filename, mode [, status])
	!使用codes_open_file，打开输出文件eps_mean.grib
	call codes_open_file(outfile, 'eps_mean.grib','w')

	! change grib headers
	! See  http://apps.ecmwf.int/codes/grib/format/grib1/local/ (definition 1 or
	! 30) and
	!      http://apps.ecmwf.int/codes/grib/format/mars/type/ for the datatype
	! Keys to change are perturbationNumber and dataType.

	! use codes_set:
	!   codes_set (gribid, key, value [, status])
	!使用codes_set，更改grib头

	call codes_set(igribout,'perturbationNumber',0)
	call codes_set(igribout,'dataType',"em")

	! change data values
	!更改数据值
	call codes_set(igribout,'values',result)

	! write message out
	! use codes_write:
	!   codes_write (gribid, ifile [, status])
	!使用codes_write，写入更改
	call codes_write(igribout,outfile)


	end program index
