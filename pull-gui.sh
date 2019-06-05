#!/usr/bin/env bash

# -------------------------------------------------------------------
# @author Lukasz Opiola
# @copyright (C) 2016 ACK CYFRONET AGH
# This software is released under the MIT license
# cited in 'LICENSE.txt'.
# -------------------------------------------------------------------
# usage:
# ./pull-gui.sh <path-to-gui-image.conf>
#
# This script copies static GUI package from a docker image to specified
# location. Requires configuration file that defines the target path and the
# docker image that should be used.
#
# Requires a gui-image config file - sh script exporting following envs:
#   IMAGE_NAME
#   IMAGE_TAG
#   PACKAGE_CHECKSUM
# -------------------------------------------------------------------

# Path relative to this script, to which static GUI package will be copied.
TARGET_PATH='_build/default/lib/gui_static.tar.gz'
# Path in GUI docker where gui package is located
PACKAGE_PATH_IN_DOCKER="/var/www/html/gui_static.tar.gz"
# Docker repository from where the image pull will be attempted at first
PRIMARY_REPO="docker.onedata.org"
# Fallback repository when the scripts fails to pull the image from PRIMARY_REPO
SECONDARY_REPO="onedata"

# If docker command is not present, just skip gui pull and continue.
command -v docker >/dev/null 2>&1 || {
    echo "WARNING: docker client not found, continuing without pulling gui." >&2;
    exit 0;
}

# Check if the config file is given
if [[ ! -f "${1}" ]]; then
    echo "Usage:"
    echo "    ./pull-gui.sh <path-to-gui-image.conf>"
    exit 1
fi

# Source gui config which should contain following exports:
IMAGE_NAME=""
IMAGE_TAG=""
PACKAGE_CHECKSUM=""
source ${1}

if [[ -z ${IMAGE_NAME} ]]; then
    echo "IMAGE_NAME not defined in ${1}, aborting"
    exit 1
fi
if [[ -z ${IMAGE_TAG} ]]; then
    echo "IMAGE_TAG not defined in ${1}, aborting"
    exit 1
fi
if [[ -z ${PACKAGE_CHECKSUM} ]]; then
    echo "PACKAGE_CHECKSUM not defined in ${1}, aborting"
    exit 1
fi

PRIMARY_IMAGE="${PRIMARY_REPO}/${IMAGE_NAME}:${IMAGE_TAG}"
SECONDARY_IMAGE="${SECONDARY_REPO}/${IMAGE_NAME}:${IMAGE_TAG}"

STATIC_FILES_IMAGE=${PRIMARY_IMAGE}
docker pull ${STATIC_FILES_IMAGE} 2>/dev/null
if [ $? -ne 0 ]; then
    echo "Cannot pull primary docker image for static GUI files - falling back to secondary"
    if [[ -z ${SECONDARY_IMAGE} ]]; then
        echo "SECONDARY_IMAGE not defined in ${1}, aborting"
        exit 1
    fi
    STATIC_FILES_IMAGE=${SECONDARY_IMAGE}
    docker pull ${STATIC_FILES_IMAGE} 2>/dev/null
    if [ $? -ne 0 ]; then
        echo "Cannot pull primary nor secondary docker image for static GUI files. Exiting."
        exit 1
    fi
fi

set -e

echo "Copying static GUI package..."
echo "    from image : ${STATIC_FILES_IMAGE}"
echo "    under path : ${TARGET_PATH}"

CONTAINER_ID=`docker run --detach ${STATIC_FILES_IMAGE} /bin/true`

docker cp -L ${CONTAINER_ID}:${PACKAGE_PATH_IN_DOCKER} ${TARGET_PATH}

docker rm -f ${CONTAINER_ID} > /dev/null

# Verify if the package checksum matches the one specified the config file
MATCH_CHECKSUM=`sha256sum ${TARGET_PATH} | grep ${PACKAGE_CHECKSUM} > /dev/null; echo $?`
if [ ${MATCH_CHECKSUM} -ne 0 ]; then
    echo "GUI package checksum does not match the one specified in config"
    echo "${PACKAGE_CHECKSUM}  expected \$PACKAGE_CHECKSUM"
    sha256sum ${TARGET_PATH}
    rm ${TARGET_PATH}
    exit 1
fi

echo "    SHA-256 sum: ${PACKAGE_CHECKSUM}"
