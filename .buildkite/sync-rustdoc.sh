#!/bin/sh

set -eu

S3_BUCKET_NAME=arret-lang-rustdoc
CLOUDFRONT_DISTRIBUTION_ID=E1FFCMKSLRZAZ

echo '--- :s3: Updating S3'
aws s3 sync --only-show-errors --delete --cache-control "max-age=3600" \
	target/doc/ "s3://${S3_BUCKET_NAME}"

echo '--- :cloudfront: Invalidating CloudFront'
aws cloudfront create-invalidation \
	--distribution-id "${CLOUDFRONT_DISTRIBUTION_ID}" \
	--paths '/*'

