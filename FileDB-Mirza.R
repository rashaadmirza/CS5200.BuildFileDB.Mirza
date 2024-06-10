# CS 5200 DBMS | ASSIGNMENT 01.1: Build File Database
# Rashaad Mohammed Mirza | mirza.ra@northeastern.edu
# May 14, 2024

# Functions defined:
# configDB(root, path): Sets up folders and the database structure.
# getExtension(fileName): Returns the file extension in uppercase.
# getFileStem(fileName): Returns the stem of the file name.
# genObjPath(root, tag): Generates the path to a file type folder.
# storeObjs(folder, root): Copies files to correct folders under root.
# - listFiles(folder): Lists all files in the specified folder.
# - copyFile(src, dest): Copies a file from source to destination.
# - createFolder(path): Creates a folder if it doesn't already exist.
# - isSubFolder(subfolder, folder): Checks if a folder is a subfolder of another folder.
# clearDB(root): Removes all folders and files in the root folder.
# main(): The main function to execute the program logic.

# 1. Successfully created R Project titled "CS5200.BuildFileDB.Mirza"

# 2. Successfully created R Script titled "FileDB-Mirza.R"

# 5. configDB(root, path)
configDB <- function(root, path = "") {
  # Check if the path is provided
  if (path == "") {
    # If path is empty, set the root path to the project folder
    rootPath <- getwd()  # Get the current working directory
  } else {
    # If path is provided, set the root path to the provided path within the project folder
    rootPath <- file.path(getwd(), path)
  }
  # Create the root folder if it doesn't exist
  if (!dir.exists(file.path(rootPath, root))) {
    dir.create(file.path(rootPath, root))
    cat("Created root folder:", file.path(rootPath, root), "\n")
  } else {
    cat("Root folder already exists:", file.path(rootPath, root), "\n")
  }
}

# 6. getExtension(fileName)
getExtension <- function(fileName) {
  extension <- tools::file_ext(fileName) # Get the file extension
  extension <- toupper(extension) # Convert extension to uppercase
  return(extension)
}
# References:
# https://stackoverflow.com/questions/7779037/extract-file-extension-from-file-path
# https://www.educative.io/answers/how-to-convert-a-string-to-uppercase-in-r

# 7. getFileStem(fileName)
getFileStem <- function(fileName) {
  stem <- tools::file_path_sans_ext(basename(fileName))
  return(stem)
}
# Reference: https://stackoverflow.com/questions/29113973/get-filename-without-extension-in-r

# 8. genObjPath(root, tag)
genObjPath <- function(root, tag) {
  
  tag <- toupper(tag) # Convert the tag to uppercase
  
  # Define a list of known extensions and their corresponding tags
  knownExtensions <- list(
    "jpg" = "JPG",
    "jpeg" = "JPG",
    "doc" = "DOC",
    "docx" = "DOC",
    "pdf" = "PDF"
  )
  
  # Get the corresponding tag from the known extensions list
  if (tag %in% names(knownExtensions)) {
    tag <- knownExtensions[[tag]]
  }
  
  objPath <- file.path(root, tag) # Generate the object path
  
  return(objPath)
}

# 9. storeObjs(folder, root)

# Function to list all files in the specified folder
listFiles <- function(folder) {
  files <- list.files(folder, full.names = TRUE)
  return(files)
}

# Function to copy a file from one location to another
copyFile <- function(src, dest) {
  file.copy(src, dest)
}

# Function to create a folder if it doesn't already exist
createFolder <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path)
  }
}

# Function to check if a folder is a subfolder of another folder
isSubfolder <- function(subfolder, folder) {
  subfolder <- normalizePath(subfolder)
  folder <- normalizePath(folder)
  return(identical(dirname(subfolder), folder))
}

# Function to store objects in their correct folders
# storeObjs <- function(folder, root) {
#   # Check if the folder is a subfolder of the root folder
#   if (isSubfolder(folder, root)) {
#     cat("Error: The folder to be copied cannot be within the root folder.\n")
#     return()
#   }
#   
#   # List all files in the folder
#   files <- listFiles(folder)
#   
#   # Iterate through each file
#   for (file in files) {
#     # Get the extension of the file
#     extension <- getExtension(file)
#     
#     # Check if the file has an extension
#     if (extension != "") {
#       # Generate the object path for the extension
#       objPath <- genObjPath(root, extension)
#       
#       # Create the folder if it doesn't exist
#       createFolder(objPath)
#       
#       # Copy the file to the correct folder
#       copyFile(file, objPath)
#       cat("Copied", file, "to", objPath, "\n")
#     }
#   }
# }

# 10. Modified storeObjs(folder, root) to take a third argument verbose that determines whether to print messages for each file copied
storeObjs <- function(folder, root, verbose = FALSE) {
  # Check if the folder is a subfolder of the root folder
  if (isSubfolder(folder, root)) {
    cat("Error: The folder to be copied cannot be within the root folder.\n")
    return()
  }
  
  # List all files in the folder
  files <- listFiles(folder)
  
  # Iterate through each file
  for (file in files) {
    # Get the extension of the file
    extension <- getExtension(file)
    
    # Check if the file has an extension
    if (extension != "") {
      # Generate the object path for the extension
      objPath <- genObjPath(root, extension)
      
      # Create the folder if it doesn't exist
      createFolder(objPath)
      
      # Copy the file to the correct folder
      copyFile(file, objPath)
      
      # Print message if verbose is TRUE
      if (verbose) {
        fileName <- getFileStem(file)
        cat("Copying", fileName, "to folder", extension, "\n")
      }
    }
  }
}

# 11. clearDB(root)
clearDB <- function(root) {
  # List all files and folders in the root directory
  allItems <- list.files(root, full.names = TRUE, recursive = TRUE)

  # Remove files
  file.remove(allItems)

  # Remove directories (including root directory)
  unlink(root, recursive = TRUE)

  # Reinitialize the database structure
  configDB(root)
  cat("Database cleared and reinitialized to a blank state\n")
}

# 4. Add a global variable before main()
rootDir <- "docDB"

# 3.3 Main function
main <- function() {
  print ("Hello, World")
  
  configDB(rootDir, )
  cat("configDB tested successfully.\n")
  
  storeObjs("testfolder", rootDir, verbose = TRUE)
  cat("storeObjs tested successfully.\n")
  
  clearDB(rootDir)
  cat("clearDB tested successfully.\n")
}

# 3.1 Calling the main function
main()

# 3.2 Calling the quit function
#quit()
