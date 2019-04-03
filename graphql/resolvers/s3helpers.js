const uuidv1 = require("uuid/v1")

function s3UploadPromise(s3, uploadParams, stream) {
  return new Promise((resolve, reject) => {
    stream.on("error", err => {
      reject("upload error")
    })

    s3.upload(uploadParams, (err, data) => {
      if (err) {
        reject(err)
      }
      if (data) {
        resolve(data)
      }
    })
  })
}

async function pipeStreamToS3(s3, bucketName, stream, id, userId) {
  // call S3 to retrieve upload file to specified bucket
  const uploadParams = {
    Bucket: bucketName,
    Key: id,
    Body: stream,
    Metadata: { userId },
  }

  const newObject = await s3UploadPromise(s3, uploadParams, stream)

  return newObject
}

module.exports = {
  pipeStreamToS3,
}
