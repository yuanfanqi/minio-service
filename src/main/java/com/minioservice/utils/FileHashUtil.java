package com.minioservice.utils;

import org.springframework.web.multipart.MultipartFile;

import java.io.InputStream;
import java.math.BigInteger;
import java.security.MessageDigest;

public class FileHashUtil {

    public String getFileHash(MultipartFile inFile) throws Exception {
        InputStream inputStream = inFile.getInputStream();
        MessageDigest md5 = MessageDigest.getInstance("MD5");
        //分多次将一个文件读入，对于大型文件而言，比较推荐这种方式，占用内存比较少。
        byte[] buffer = new byte[1024];
        int length = -1;
        while ((length = inputStream.read(buffer, 0, 1024)) != -1) {
            md5.update(buffer, 0, length);
        }
        inputStream.close();
        byte[] md5Bytes = md5.digest();
        BigInteger bigInt = new BigInteger(1, md5Bytes);
        return bigInt.toString(16);
    }
}
