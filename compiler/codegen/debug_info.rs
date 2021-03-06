use std::collections::HashMap;
use std::os::unix::ffi::OsStrExt;
use std::{env, ffi, ptr};

use codespan_reporting::files::Files as _;

use llvm_sys::core::*;
use llvm_sys::debuginfo::*;
use llvm_sys::prelude::*;

use arret_syntax::datum::DataStr;
use arret_syntax::span::{FileId, Span};

use crate::source::SourceLoader;

pub struct DebugInfoBuilder<'sl> {
    pub llvm_dib: LLVMDIBuilderRef,

    source_loader: &'sl SourceLoader,
    current_dir: ffi::OsString,
    file_metadata: HashMap<FileId, LLVMMetadataRef>,
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
            .map(|current_dir| current_dir.as_os_str().to_owned())
            .unwrap_or_else(ffi::OsString::new);

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
        let main_file_id = if let Some(file_id) = main_span.file_id() {
            file_id
        } else {
            return;
        };

        let main_file_metadata = self.file_metadata(main_file_id);

        let producer = b"arret\0";

        unsafe {
            // This is implicitly added to the LLVM module
            LLVMDIBuilderCreateCompileUnit(
                self.llvm_dib,
                LLVMDWARFSourceLanguage::LLVMDWARFSourceLanguageC,
                main_file_metadata,
                producer.as_ptr() as *const _,
                producer.len(),
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

    pub fn file_metadata(&mut self, file_id: FileId) -> LLVMMetadataRef {
        if let Some(metadata) = self.file_metadata.get(&file_id) {
            return *metadata;
        }

        let filename = self.source_loader.files().name(file_id).unwrap();

        let metadata = unsafe {
            LLVMDIBuilderCreateFile(
                self.llvm_dib,
                filename.as_ptr() as *const _,
                filename.len(),
                self.current_dir.as_bytes().as_ptr() as *const _,
                self.current_dir.as_bytes().len(),
            )
        };

        self.file_metadata.insert(file_id, metadata);
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
                LLVMDIFlagZero,
            )
        }
    }

    pub fn add_function_debug_info(
        &mut self,
        span: Span,
        source_name: Option<&DataStr>,
        llvm_function: LLVMValueRef,
    ) {
        let file_id = if let Some(file_id) = span.file_id() {
            file_id
        } else {
            return;
        };

        let location = if let Ok(location) = self
            .source_loader
            .files()
            .location(file_id, span.start() as usize)
        {
            location
        } else {
            return;
        };

        let line_index = location.line_number - 1;

        let file_metadata = self.file_metadata(file_id);

        unsafe {
            let mut linkage_name_len: usize = 0;
            let linkage_name_ptr = LLVMGetValueName2(llvm_function, &mut linkage_name_len);

            let function_metadata = LLVMDIBuilderCreateFunction(
                self.llvm_dib,
                file_metadata, // `Scope`
                source_name
                    .map(|source_name| source_name.as_ptr() as *const _)
                    .unwrap_or(linkage_name_ptr),
                source_name
                    .as_ref()
                    .map(|source_name| source_name.len())
                    .unwrap_or(linkage_name_len),
                linkage_name_ptr,
                linkage_name_len,
                file_metadata,
                line_index as u32,
                self.placeholder_subroutine_type(file_metadata),
                source_name.is_none() as i32, // `IsLocalToUnit`
                1,                            // `IsDefinition`
                line_index as u32,            // `ScopeLine`
                LLVMDIFlagZero,
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
