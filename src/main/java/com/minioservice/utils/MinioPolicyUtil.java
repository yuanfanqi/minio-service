package com.minioservice.utils;

/**
 * @Description: 桶策略设置
 * @Author: song
 * @Date: 2023/2/7 15:56
 */
public class MinioPolicyUtil {
    public String getPolicy (String bucket) {
        return  " {\n" +
                "     \"Statement\": [\n" +
                "         {\n" +
                "             \"Action\": [\n" +
                "                 \"s3:GetBucketLocation\",\n" +
                "                 \"s3:ListBucket\"\n" +
                "             ],\n" +
                "             \"Effect\": \"Allow\",\n" +
                "             \"Principal\": \"*\",\n" +
                "             \"Resource\": \"arn:aws:s3:::"+ bucket +"\"\n" +
                "         },\n" +
                "         {\n" +
                "             \"Action\": \"s3:GetObject\",\n" +
                "             \"Effect\": \"Allow\",\n" +
                "             \"Principal\": \"*\",\n" +
//                "             \"Resource\": \"arn:aws:s3:::"+ bucket +"/myobject*\"\n" +
                "             \"Resource\": \"arn:aws:s3:::"+ bucket +"/**\"\n" +
                "         }\n" +
                "     ],\n" +
                "     \"Version\": \"2012-10-17\"\n" +
                "}";
    }

    // Assume policyJson contains below JSON string;
//    {
//        "Statement": [
//        {
//            "Action": [
//            "s3:GetBucketLocation",
//                    "s3:ListBucket"
//             ],
//            "Effect": "Allow",
//                "Principal": "*",
//                "Resource": "arn:aws:s3:::my-bucketname"
//        },
//        {
//            "Action": "s3:GetObject",
//                "Effect": "Allow",
//                "Principal": "*",
//                "Resource": "arn:aws:s3:::my-bucketname/myobject*"
//        }
//     ],
//        "Version": "2012-10-17"
//    }
}
