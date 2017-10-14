from __future__ import print_function
import os
import sys
import argparse
import boto3
from botocore.exceptions import ClientError
from botocore.client import Config
import mimetypes


def upload_to_s3(bucket, artefact, is_folder, bucket_key):
    try:
        client = boto3.client('s3', config=Config(signature_version='s3v4'))
    except ClientError as err:
        print("Failed to create boto3 client.\n" + str(err))
        return False
    if is_folder == 'true':
        for root, dirs, files in os.walk(artefact, topdown=False):
            print('Walking it, root: ' + root)
            s3_root = root.replace(artefact + '/', '')
            s3_root = s3_root.replace(artefact, '')
            print('S3-root', s3_root)
            for filename in files:
                content_type = guess_content_type(filename)
                try:
                    print(filename)
                    client.upload_file(os.path.join(root, filename), bucket, os.path.join(s3_root, filename),
                                       ExtraArgs={'ContentType': content_type})
                except ClientError as err:
                    print("Failed to upload artefact to S3.\n" + str(err))
                    return False
                except IOError as err:
                    print("Failed to access artefact in this directory.\n" + str(err))
                    return False
    else:
        print('Uploading file ' + artefact)
        client.upload_file(artefact, bucket, bucket_key)
    return True


def guess_content_type(filename):
    if filename.endswith('html'):
        return 'text/html'
    elif filename.endswith('js'):
        return 'application/json'
    elif filename.endswith('css'):
        return 'text/css'
    elif filename.endswith('map'):
        return 'application/octet-stream'
    elif filename.endswith('webapp'):
        return 'application/x-web-app-manifest+json'
    else:
        return mimetypes.guess_type(filename)[0]


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("bucket", help="Name of the existing S3 bucket")
    parser.add_argument("artefact", help="Name of the artefact to be uploaded to S3")
    parser.add_argument("is_folder", help="True if its the name of a folder")
    parser.add_argument("bucket_key", help="Name of file in bucket")
    args = parser.parse_args()

    if not upload_to_s3(args.bucket, args.artefact, args.is_folder, args.bucket_key):
        sys.exit(1)


if __name__ == "__main__":
    main()