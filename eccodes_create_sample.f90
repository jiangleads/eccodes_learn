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
	!               Creation of grib message, from a grib sample (or template).
	!
	! 描述: 怎样从一个文件创建和使用index来访问消息
	!       从一个grib样本（或模板）创建grib消息
	!
	program index
	use eccodes
	implicit none

	integer              :: iret
	integer,dimension(:),allocatable :: paramId,number
	character(len=20)    :: packingType
	character(len=5)     :: marsStream
	integer              :: paramIdSize,numberSize
	integer              :: i,j,is_missing
	integer              :: idx,igrib,igribout,count,numberOfValues
	integer              :: level,date,time,stepUnits,stepRange,startStep,endStep
	integer              :: localDefinitionNumber,Ni,Nj,PLPresent,N,nb_pl
	integer              :: generatingProcessIdentifier,numberOfForecastsInEnsemble
	integer              :: legBaseDate,legBaseTime,legNumber
	integer              :: bitmapPresent,bitsPerValue
	integer              :: outfile
	real (KIND=8),dimension(:), allocatable   :: values,result
	real (KIND=8)        :: latitudeOfFirstPoint,longitudeOfFirstPoint
	real (KIND=8)        :: latitudeOfLastPoint,longitudeOfLastPoint
	integer,dimension(:), allocatable   :: pl

	! create an index for the file 'eps' file using the keys number and paramId
	! use codes_index_create
	! 使用codes_index_create函数，根据关键字number和paramId，创建文件'eps'的索引
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

	! take a sample grib message
	! use codes_grib_new_from_samples:
	!   codes_grib_new_from_samples (gribid, samplename [, status])
    !使用codes_grib_new_from_samples函数，读取一个样本grib消息
	call codes_grib_new_from_samples(igribout, "reduced_gg_pl_grib1")

	! open output grib file eps_mean.grib
	! use codes_open_file:
	!   codes_open_file (ifile, filename, mode [, status])
	!使用codes_open_file，打开输出文件eps_mean.grib
	call codes_open_file(outfile, 'eps_mean.grib','w')

	! change grib headers:
	! 更改grib头
	! 1. product definition:
	!
	! change grib headers
	! See  http://apps.ecmwf.int/codes/grib/format/grib1/local/ (definition 1 or
	! 30) and
	!      http://apps.ecmwf.int/codes/grib/format/mars/type/ for the datatype
	! Keys to change are perturbationNumber and dataType.

	! use codes_set:
	!   codes_set (gribid, key, value [, status])

	call codes_set(igribout,'paramId',130) !设置写出变量的paramId为130（表示温度，和之前读入的的一致）
	
	call codes_get(igrib,'level',level)    
	call codes_set(igribout,'level',level)
	
	call codes_get(igrib,'dataDate',date)
	call codes_set(igribout,'dataDate',date)
	
	call codes_get(igrib,'dataTime',time)
	call codes_set(igribout,'dataTime',time)
	
	call codes_get(igrib,'stepUnits',stepUnits)
	call codes_set(igribout,'stepUnits',stepUnits)
	
	call codes_get(igrib,'stepRange',stepRange)
	call codes_set(igribout,'stepRange',stepRange)
	
	call codes_get(igrib,'startStep',startStep)
	call codes_set(igribout,'startStep',startStep)
	
	call codes_get(igrib,'endStep',endStep)
	call codes_set(igribout,'endStep',endStep)

	call codes_set(igribout,'dataType',"em")
	
	call codes_get(igrib,'generatingProcessIdentifier',generatingProcessIdentifier)
	call codes_set(igribout,'generatingProcessIdentifier',generatingProcessIdentifier)
	
	call codes_get(igrib,'localDefinitionNumber',localDefinitionNumber)
	call codes_set(igribout,'localDefinitionNumber',localDefinitionNumber)
	
	call codes_get(igrib,'numberOfForecastsInEnsemble',numberOfForecastsInEnsemble)
	call codes_set(igribout,'numberOfForecastsInEnsemble',numberOfForecastsInEnsemble)
	
	call codes_get(igrib,'marsStream',marsStream)
	call codes_set(igribout,'marsStream',marsStream)

	call codes_get(igrib,'legBaseDate',legBaseDate)
	call codes_set(igribout,'legBaseDate',legBaseDate)
	
	call codes_get(igrib,'legBaseTime',legBaseTime)
	call codes_set(igribout,'legBaseTime',legBaseTime)
	
	call codes_get(igrib,'legNumber',legNumber)
	call codes_set(igribout,'legNumber',legNumber)

	! 2. grid definition: 网格定义
	!
	! see https://software.ecmwf.int/wiki/display/GRIB/Gaussian+grids

	is_missing=0;
	call codes_is_missing(igrib,'Ni',is_missing);
	if ( is_missing /= 1 ) then
		call codes_get(igrib,'Ni',Ni)
		call codes_set(igribout,'Ni',Ni)
	endif

	is_missing=0;
	call codes_is_missing(igrib,'Nj',is_missing);
	if ( is_missing /= 1 ) then
		call codes_get(igrib,'Nj',Nj)
		call codes_set(igribout,'Nj',Nj)
	endif

	call codes_get(igrib,'latitudeOfFirstGridPointInDegrees',latitudeOfFirstPoint)
	call codes_set(igribout,'latitudeOfFirstGridPointInDegrees',latitudeOfFirstPoint)
	
	call codes_get(igrib,'longitudeOfFirstGridPointInDegrees',longitudeOfFirstPoint)
	call codes_set(igribout,'longitudeOfFirstGridPointInDegrees',longitudeOfFirstPoint)
	
	call codes_get(igrib,'latitudeOfLastGridPointInDegrees',latitudeOfLastPoint)
	call codes_set(igribout,'latitudeOfLastGridPointInDegrees',latitudeOfLastPoint)
	
	call codes_get(igrib,'longitudeOfLastGridPointInDegrees',longitudeOfLastPoint)
	call codes_set(igribout,'longitudeOfLastGridPointInDegrees',longitudeOfLastPoint)
	
	call codes_get(igrib,'N',N)
	call codes_set(igribout,'N',N)
	
	call codes_get(igrib,'PLPresent',PLPresent)
	if (PLPresent == 1) then
		call codes_get_size(igrib,'pl',nb_pl)
		allocate(pl(nb_pl))
		call codes_get(igrib,'pl',pl)
		call codes_set(igribout,'pl',pl)
		deallocate(pl)
	else
		print*, "There is no PL values in your GRIB message!"
	end if

	!  3. bitmap definition:

	call codes_get(igrib,'bitmapPresent',bitmapPresent)
	call codes_set(igribout,'bitmapPresent',bitmapPresent)

	! 4. data:

	call codes_get(igrib,'bitsPerValue',bitsPerValue)
	call codes_set(igribout,'bitsPerValue',bitsPerValue)
	call codes_get(igrib,'packingType',packingType)
	call codes_set(igribout,'packingType',packingType)

	call codes_set(igribout,'values',result)

	! write message out
	! use codes_write:
	!   codes_write (gribid, ifile [, status])

	call codes_write(igribout,outfile)

	! release the output grib message
	! use grib_release

	call codes_release(igribout)


	end program index
