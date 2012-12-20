PRO mainTest, argument

 help, argument

END

PRO delta

 !P.COLOR=0
 fairModeApp=obj_new("FMApplication")
 fairModeApp->startUp
 fairModeApp->display
 ;xmanager, CATCH=0, /NO_BLOCK

END

PRO buildTestConfiguration, WRITE=WRITE

 print, "++++++++++Buildings test files...++++++++++++++++++++++++++++++++++++++++++++++++"
 fsm=obj_new("FMFileSystemManager")
 fsm->test_BuildInternalSystemConfig, WRITE=WRITE
 fsm->test_BuildResourceConfigFiles, WRITE=WRITE
 obj_destroy, fsm

END

PRO buildTestBenchMark

 print, "++++++++++Buildings test files...++++++++++++++++++++++++++++++++++++++++++++++++"
 testBen=obj_new('BenchmarkInfo')
 batchFiles=['testscatter.rqs', 'testdef.rqs', 'testbugle.rqs']
 testBen->setBatchFileNames, batchFiles
 testBen->setDescription, 'Test purpose only'
 testBen->setOutputFileNameDestination, 'benchmark1'
 testBen->setOutputFormat, 'IMAGE'
 testBen->setOutputFormatType, 'bmp'
 testBen->saveData, 'E:\mirko\fairmode\save\test1BM.bmk'
 obj_destroy, testBen

END