use std::collections::HashMap;
use std::os::unix::ffi::OsStrExt;
use std::{env, ffi, ptr};

use llvm_sys::core::*;
use llvm_sys::debuginfo::*;
use llvm_sys::prelude::*;

use syntax::span::Span;

use crate::source::{SourceFileId, SourceKind, SourceLoader, SourceLoc};

pub struct DebugInfoBuilder<'sl> {
    pub llvm_dib: LLVMDIBuilderRef,

    source_loader: &'sl SourceLoader,
    current_dir: ffi::CString,
    file_metadata: HashMap<SourceFileId, Option<LLVMMetadataRef>>,
}

impl<'sl> DebugInfoBuilder<'sl> {
    pub fn new(
        source_loader: &'sl SourceLoader,
        optimised: bool,
        main_span: Span,
        module: LLVMModuleRef,
    ) -> DebugInfoBuilder<'sl> {
        // This is needed for all of our file metadata so the debugger can resolve relative paths
        let current_dir = env::current_dir()
            .ok()
            .map(|current_dir| ffi::CString::new(current_dir.as_os_str().as_bytes()).unwrap())
            .unwrap_or_else(|| ffi::CString::new("").unwrap());

        let llvm_dib = unsafe { LLVMCreateDIBuilderDisallowUnresolved(module) };

        let mut di_builder = DebugInfoBuilder {
            llvm_dib,

            source_loader,
            current_dir,
            file_metadata: HashMap::new(),
        };

        di_builder.add_compile_unit_metadata(optimised, main_span);
        di_builder
    }

    fn add_compile_unit_metadata(&mut self, optimised: bool, main_span: Span) {
        let main_loc = SourceLoc::from_byte_index(self.source_loader, main_span.start());
        let main_file_id = main_loc.source_file_id();
        let main_file_metadata = if let Some(metadata) = self.file_metadata(main_file_id) {
            metadata
        } else {
            return;
        };

        let producer = ffi::CString::new("arret").unwrap();

        unsafe {
            // This is implicitly added to the LLVM module
            LLVMDIBuilderCreateCompileUnit(
                self.llvm_dib,
                LLVMDWARFSourceLanguage::LLVMDWARFSourceLanguageC,
                main_file_metadata,
                producer.as_ptr(),
                producer.as_bytes().len(),
                optimised as i32,                                 // `isOptimized`
                ptr::null(),                                      // `Flags`
                0,                                                // `FlagsLen`
                0,                                                // `RuntimeVer`
                ptr::null(),                                      // `SplitName`
                0,                                                // `SplitNameLen`
                LLVMDWARFEmissionKind::LLVMDWARFEmissionKindFull, // `LLVMDWARFEmissionKind::LLVMDWARFEmissionKindLineTablesOnly`,
                0,                                                // `DWOId`
                0,                                                // `SplitDebugInlining`
                0,                                                // `DebugInfoForProfiling`
            );
        }
    }

    pub fn file_metadata(&mut self, source_file_id: SourceFileId) -> Option<LLVMMetadataRef> {
        if let Some(metadata) = self.file_metadata.get(&source_file_id) {
            return *metadata;
        }

        let source_file = self.source_loader.source_file(source_file_id);

        let metadata = if let SourceKind::File(ref filename) = source_file.kind() {
            ffi::CString::new(filename.as_bytes())
                .ok()
                .map(|c_filename| unsafe {
                    LLVMDIBuilderCreateFile(
                        self.llvm_dib,
                        c_filename.as_ptr() as *const _,
                        c_filename.as_bytes().len(),
                        self.current_dir.as_ptr() as *const _,
                        self.current_dir.as_bytes().len(),
                    )
                })
        } else {
            None
        };

        self.file_metadata.insert(source_file_id, metadata);
        metadata
    }

    /// Returns a subroutine type containing no parameters
    pub fn placeholder_subroutine_type(
        &mut self,
        file_metadata: LLVMMetadataRef,
    ) -> LLVMMetadataRef {
        // This includes no parameter types
        unsafe {
            LLVMDIBuilderCreateSubroutineType(
                self.llvm_dib,
                file_metadata,
                ptr::null_mut(),
                0,
                LLVMDIFlags::LLVMDIFlagZero,
            )
        }
    }

    pub fn add_function_debug_info(
        &mut self,
        span: Span,
        source_name: Option<&String>,
        llvm_function: LLVMValueRef,
    ) {
        let source_loc = SourceLoc::from_byte_index(self.source_loader, span.start());
        let source_file_id = source_loc.source_file_id();
        let source_line = source_loc.line();

        let file_metadata = if let Some(file_metadata) = self.file_metadata(source_file_id) {
            file_metadata
        } else {
            return;
        };

        unsafe {
            let c_source_name =
                source_name.map(|source_name| ffi::CString::new(source_name.as_bytes()).unwrap());

            let mut linkage_name_len: usize = 0;
            let linkage_name_ptr = LLVMGetValueName2(llvm_function, &mut linkage_name_len);

            let function_metadata = LLVMDIBuilderCreateFunction(
                self.llvm_dib,
                file_metadata, // `Scope`
                c_source_name
                    .as_ref()
                    .map(|c_source_name| c_source_name.as_ptr())
                    .unwrap_or(linkage_name_ptr),
                c_source_name
                    .as_ref()
                    .map(|c_source_name| c_source_name.as_bytes().len())
                    .unwrap_or(linkage_name_len),
                linkage_name_ptr,
                linkage_name_len,
                file_metadata,
                source_line as u32,
                self.placeholder_subroutine_type(file_metadata),
                source_name.is_none() as i32, // `IsLocalToUnit`
                1,                            // `IsDefinition`
                source_line as u32,           // `ScopeLine`
                LLVMDIFlags::LLVMDIFlagZero,
                1, // `IsOptimized`
            );

            LLVMSetSubprogram(llvm_function, function_metadata);
        }
    }

    pub fn finalise(&mut self) {
        unsafe {
            LLVMDIBuilderFinalize(self.llvm_dib);
        }
    }
}

impl Drop for DebugInfoBuilder<'_> {
    fn drop(&mut self) {
        unsafe { LLVMDisposeDIBuilder(self.llvm_dib) }
    }
}
