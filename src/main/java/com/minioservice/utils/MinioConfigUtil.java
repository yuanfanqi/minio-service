package com.minioservice.utils;

import io.minio.MinioClient;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;

/**
 * @Description: minio的配置信息
 * @Author: song
 * @Date: 2023/2/6 16:23
 */
@Component
public class MinioConfigUtil {
    public static String minioUrl;
    public static String minioAdm;
    public static String minioPsw;

    @Value("${minio.endpoint}")
    private String endpoint;
    @Value("${minio.accesskey}")
    private String accesskey;
    @Value("${minio.secretkey}")
    private String secretkey;

    @PostConstruct
    public void init() {
        minioUrl = this.endpoint;
        minioAdm = this.accesskey;
        minioPsw = this.secretkey;
    }

    public MinioClient getMinioClient() {
        return new MinioClient.Builder().endpoint(minioUrl).credentials(minioAdm, minioPsw).build();
    }

    public String getDelBucket(String bucket) {
        return bucket + "del";
    }
}
