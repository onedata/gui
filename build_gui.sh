#!/usr/bin/env bash

set -e

# Directory where this script resides
THIS_SCRIPT_DIR=$(cd ${0%/*} && pwd)
# the name of this script
SCRIPT_NAME=`basename $0`
# Predefined file name of gui config
GUI_CONFIG_NAME="gui.config"
# Path to nvm binary
NVM_BINARY=/usr/lib/nvm/nvm.sh

usage_and_exit() {
    echo "Usage: ${0} (dev | prod | help) [rebar_config_path] "
    echo "      dev - build a development release of GUI (this includes for example live-reload of changed pages)."
    echo "      prod - build a production release of GUI."
    echo "      help - display this help message."
    echo "      rebar_config_path - optional path to rebar.config in parent project (project using the gui dep)."
    echo "          If not provided, the script will assume it resides in the parent project root under name 'rebar.config'"
    exit 1
}

info_msg() {
    echo "[GUI BUILDER] ${1}"
}

# Root directory of the project that uses gui as a dep
INCLUDER_PROJECT_DIR="${THIS_SCRIPT_DIR}/../.."

# Check if it is a production or dev build
if [ "${1}" == "prod" ]; then
    BUILD_COMMAND="ember build -prod"
elif [ "${1}" == "dev" ]; then
    BUILD_COMMAND="ember build -dev"
elif [ "${1}" == "help" ]; then
    usage_and_exit
else
    usage_and_exit
fi

# Resolve the path to parent project's rebar.config
if [ -z "${2}" ]; then
    REBAR_CONFIG="${INCLUDER_PROJECT_DIR}/rebar.config"
else
    REBAR_CONFIG=${1}
fi

if [ -f ${REBAR_CONFIG} ];
then
   :
else
   echo "Cannot find rebar.config in parent dir."
   usage_and_exit
fi

# Resolve target release dir of parent project based on rebar.config and reltool.config
REL_DIR=$(cat ${REBAR_CONFIG} | grep sub_dirs | awk -F '"' '{print $2}')
REL_DIR="${INCLUDER_PROJECT_DIR}/${REL_DIR}"
REL_TARGET_DIR=$(cat ${REL_DIR}/reltool.config | grep target_dir | awk -F '"' '{print $2}')
REL_TARGET_DIR="${REL_DIR}/${REL_TARGET_DIR}"

# Resolve the path to gui.config file in parent project
GUI_CONFIG="${REL_DIR}/${GUI_CONFIG_NAME}"
if [ -f ${GUI_CONFIG} ];
then
    info_msg "gui.config found, building GUI..."
    if [ "${1}" == "prod" ]; then
        info_msg "A production release of GUI will be built."
    fi
else
    info_msg "gui.config was not found, NOT building GUI."
    exit 0
fi

# Resolve the source and release dir of GUI
SOURCE_GUI_DIR=$(cat ${GUI_CONFIG} | grep source_gui_dir | awk -F '"' '{print $2}')
info_msg "Source GUI dir:  ${SOURCE_GUI_DIR}"
SOURCE_GUI_DIR="${INCLUDER_PROJECT_DIR}/${SOURCE_GUI_DIR}"
RELEASE_GUI_DIR=$(cat ${GUI_CONFIG} | grep release_gui_dir | awk -F '"' '{print $2}')
info_msg "Release GUI dir: ${RELEASE_GUI_DIR}"
RELEASE_GUI_DIR="${REL_TARGET_DIR}/${RELEASE_GUI_DIR}"

# Create a tmp dir for GUI build. Broccoli expects it to be in tmp dir in gui app,
# but we do not want to let docker write there.
# Then, make it a symlink to dir in /tmp
#mkdir /tmp/broccoli_tmp
#cd ${SOURCE_GUI_DIR} && ln -s /tmp/broccoli_tmp tmp

# Source NVM
. ${NVM_BINARY}
# Make sure NVM loads node binaries
nvm use default node
# Enter source GUI directory
cd ${SOURCE_GUI_DIR}
# Install node dependencies
npm install
# Install bower dependencies
bower install
# Do the build
${BUILD_COMMAND} --output-path=${RELEASE_GUI_DIR}
