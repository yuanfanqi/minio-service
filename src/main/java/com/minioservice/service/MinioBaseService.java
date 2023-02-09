package com.minioservice.service;

import com.minioservice.entity.LogMinioOperate;
import com.minioservice.enums.CommonEnum;
import com.minioservice.enums.FileOperateEnum;
import com.minioservice.utils.FileHashUtil;
import com.minioservice.utils.MinioConfigUtil;
import com.minioservice.utils.MinioPolicyUtil;
import io.minio.*;
import io.minio.messages.Item;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.InputStream;
import java.util.*;

/**
 * @Author: song
 * @Date: 2023/2/3 11:18
 * @Description:
 */
@Service("minioBaseService")
public class MinioBaseService {

    private final static Logger logger = LoggerFactory.getLogger(MinioBaseService.class);

    @Autowired
    LogMinioOperateService logMinioOperateService;

     /**
     * 文件上传
     * isReplace[false, true]
     * @param: [inFile, prodCode, token]
     * @return: Map<String, String>
     * @Author: song
     * @Date: 2023/2/3 11:27
     */
    public Map<String, String> doUpload(MultipartFile inFile, String prodCode, String token, String desc, boolean isReplace) throws Exception {
        Map<String, String> res = new HashMap<String, String>();
        //todo token获取用户信息
        logger.info("=================文件上传：服务" + prodCode + "，操作用户");
        if (inFile.isEmpty()) {
            logger.info("上传失败：上传文件不能为空");
            res.put("STATUS", "E");
            res.put("MSG", "上传失败：上传文件不能为空");
            return res;
        }
        res = uploadForeach(inFile, prodCode, desc, isReplace);
        return res;
    }

    /**
     * 文件删除
     * @param: [fileName, prodCode, token, isLogicDel]
     * @return: java.lang.String
     * @Author: song
     * @Date: 2023/2/6 13:43
     */
    public Map<String, String> removeFile (String fileName, String prodCode, String token, boolean isLogicDel) throws Exception {
        Map<String, String> res = new HashMap<String, String>();

        MinioConfigUtil minioConfigUtil = new MinioConfigUtil();
        MinioClient minioClient = minioConfigUtil.getMinioClient();
        if (!minioClient.bucketExists(BucketExistsArgs.builder().bucket(prodCode).build())) {
            res.put("STATUS", "E");
            res.put("MSG", "删除失败：该文件不存在");
            return res;
        }
        //判断文件是否已存在:数据库
        List<LogMinioOperate> fileExist = logMinioOperateService.isFileExist(fileName, null, prodCode);
        if (fileExist.isEmpty()) {
            res.put("STATUS", "E");
            res.put("MSG", "删除失败：该文件不存在");
            return res;
        }
        //判断文件是否已存在
        StatObjectResponse statObject = null;
        try {
            statObject = minioClient.statObject(StatObjectArgs.builder().bucket(prodCode).object(fileName).build());
        } catch (Exception e) {
            statObject = null;
        }
        if (null == statObject) {
            res.put("STATUS", "E");
            res.put("MSG", "删除失败：该文件不存在");
            return res;
        }

        LogMinioOperate deleteParam = new LogMinioOperate();
        if (isLogicDel) {
            String delBucket = minioConfigUtil.getDelBucket(prodCode);
            //判断服务桶目录是否存在
            if (!minioClient.bucketExists(BucketExistsArgs.builder().bucket(delBucket).build())) {
                minioClient.makeBucket(MakeBucketArgs.builder().bucket(delBucket).build());
                MinioPolicyUtil policyUtil = new MinioPolicyUtil();
                minioClient.setBucketPolicy(SetBucketPolicyArgs.builder().bucket(delBucket).config(policyUtil.getPolicy(delBucket)).build());
            }
            //逻辑删除--可移动到备份文件夹
            minioClient.copyObject(CopyObjectArgs.builder().bucket(delBucket).object(fileName).source(CopySource.builder().bucket(prodCode).object(fileName).build()).build());
            deleteParam.setSourceService(delBucket);
        }
        deleteParam.setIsDelete(CommonEnum.TRUE.getStatusVal());
        deleteParam.setModifyUser("231");
        deleteParam.setId(fileExist.get(0).getId());
        minioClient.removeObject(RemoveObjectArgs.builder().bucket(prodCode).object(fileName).build());
        logMinioOperateService.update(deleteParam);
        res.put("STATUS", "S");
        res.put("MSG", "删除成功");
        return res;
    }

    public List<String> getBucketFileList(String prodCode, String token) throws Exception {
        ArrayList<String> res = new ArrayList<>();

        MinioConfigUtil minioConfigUtil = new MinioConfigUtil();
        MinioClient minioClient = minioConfigUtil.getMinioClient();
        Iterable<Result<Item>> listObjects = minioClient.listObjects(ListObjectsArgs.builder().bucket(prodCode).build());
        for (Result<Item> fileItem : listObjects) {
            res.add(fileItem.get().objectName());
        }
        logger.info("查询成功，" + prodCode + "共有" + res.size() + "个文件");
        return res;
    }

    /**
     * @Description: 文件下载
     * @param: [fileName, prodCode, token, path]
     * @return: java.util.Map<java.lang.String,java.lang.String>
     * @Author: song
     * @Date: 2023/2/8 17:38
     */
    public Map<String, String> doDownload (String fileName, String prodCode, String token, String path) {
        Map<String, String> res = new HashMap<String, String>();
        res.put("STATUS", "S");
        res.put("MSG", "下载成功");

        //判断文件是否已存在:数据库
        List<LogMinioOperate> fileExist = logMinioOperateService.isFileExist(fileName, null, prodCode);
        if (fileExist.isEmpty()) {
            res.put("STATUS", "E");
            res.put("MSG", "删除失败：该文件不存在");
            return res;
        }

        MinioConfigUtil minioConfigUtil = new MinioConfigUtil();
        MinioClient minioClient = minioConfigUtil.getMinioClient();
        StatObjectResponse statObject = null;
        try {
            statObject = minioClient.statObject(StatObjectArgs.builder().bucket(prodCode).object(fileName).build());
            if (null == statObject) {
                res.put("STATUS", "E");
                res.put("MSG", "下载失败：文件不存在");
            }
        } catch (Exception e) {
            res.put("STATUS", "E");
            res.put("MSG", "下载失败：" + e.getMessage());
        }
        path = path + "/" + statObject.object();
        try {
            minioClient.downloadObject(DownloadObjectArgs.builder().bucket(prodCode).object(fileName).filename(path).build());
        } catch (Exception e) {
            res.put("STATUS", "E");
            res.put("MSG", "下载失败：" + e.getMessage());
        }
        logger.info("=================文件下载完成=================");
        //更新数据库（下载
        LogMinioOperate modifyParam = new LogMinioOperate();
        modifyParam.setModifyUser("123");
        modifyParam.setOperate(FileOperateEnum.DOWNLOAD.getVal());
        modifyParam.setId(fileExist.get(0).getId());
        logMinioOperateService.update(modifyParam);
        return res;
    }

    /**
     * @Description: 多文件上传
     * @param: [inFileList, prodCode, token, desc, isReplace]
     * @return: java.util.Map<java.lang.String,java.lang.String>
     * @Author: song
     * @Date: 2023/2/9 13:55
     */
    public Map<String, String> doUploadFiles(MultipartFile[] inFileList, String prodCode, String token, String desc, boolean isReplace) throws Exception {
        Map<String, String> res = new HashMap<String, String>();
        //todo token获取用户信息
        logger.info("=================多文件上传：服务" + prodCode + "，操作用户");

        if (null == inFileList || 0 == inFileList.length) {
            logger.info("上传失败：上传文件不能为空");
            res.put("STATUS", "E");
            res.put("MSG", "上传失败：上传文件不能为空");
            return res;
        }
        List<String> urlList = new ArrayList<>();
        String errMsg = "上传失败：";
        for (MultipartFile file : inFileList) {
            Map<String, String> fileRes = uploadForeach(file, prodCode, desc, isReplace);
            if ("S".equals(fileRes.get("STATUS"))) {
                urlList.add(fileRes.get("MSG"));
            } else {
                errMsg += file.getOriginalFilename();
            }
        }
        logger.info(errMsg);
        if (urlList.isEmpty()) {
            res.put("STATUS", "E");
            res.put("MSG", errMsg);
        } else {
            res.put("STATUS", "S");
            res.put("MSG", urlList.toString());
        }
        return res;
    }
    //多文件下载
    //多文件删除
    //文件tags的修改

    /**
     * @Description: 单个文件上传操作
     * @param: [inFile, prodCode, desc, isReplace]
     * @return: java.util.Map<java.lang.String,java.lang.String>
     * @Author: song
     * @Date: 2023/2/9 11:11
     */
    private Map<String, String> uploadForeach (MultipartFile inFile, String prodCode, String desc, boolean isReplace) throws Exception {
        Map<String, String> res = new HashMap<String, String>();
        //上传文件前：判断是否存在同样的图片（通过hash+fileName）
        FileHashUtil fileHashUtil = new FileHashUtil();
        String fileHash = fileHashUtil.getFileHash(inFile);
        List<LogMinioOperate> fileExist = logMinioOperateService.isFileExist(inFile.getOriginalFilename(), fileHash, prodCode);
        if (0 < fileExist.size() && !isReplace) {
            logger.info("上传失败：文件已存在，且未授权覆盖");
            res.put("STATUS", "E");
            res.put("MSG", "上传失败：文件已存在，且未授权覆盖");
            return res;
        }
        //end  上传文件前：判断是否存在同样的图片（通过hash+fileName）
        //minio文件上传
        MinioConfigUtil minioConfigUtil = new MinioConfigUtil();
        MinioClient minioClient = null;
        try {
            minioClient = minioConfigUtil.getMinioClient();
        } catch (Exception e) {
            logger.error("上传失败：minIO建立连接失败");
            res.put("STATUS", "E");
            res.put("MSG", "minIO建立连接失败");
            return res;
        }
        //判断服务桶目录是否存在
        if (!minioClient.bucketExists(BucketExistsArgs.builder().bucket(prodCode).build())) {
            minioClient.makeBucket(MakeBucketArgs.builder().bucket(prodCode).build());
            MinioPolicyUtil policyUtil = new MinioPolicyUtil();
            minioClient.setBucketPolicy(SetBucketPolicyArgs.builder().bucket(prodCode).config(policyUtil.getPolicy(prodCode)).build());
        }
        //判断文件是否已存在(通过文件名)
        StatObjectResponse statObject = null;
        String infoRes = null;
        try {
            statObject = minioClient.statObject(StatObjectArgs.builder().bucket(prodCode).object(inFile.getOriginalFilename()).build());
        } catch (Exception e) {
            statObject = null;
        }
        if (null != statObject && !isReplace) {
            infoRes = "文件名已存在，且未授权覆盖";
            logger.info("=================文件上传失败：" + inFile.getOriginalFilename() + infoRes);
            res.put("STATUS", "E");
            res.put("MSG", infoRes);
            return res;
        }
        InputStream inputStream = inFile.getInputStream();
        //上传新文件对象
        minioClient.putObject(PutObjectArgs.builder().bucket(prodCode).object(inFile.getOriginalFilename()).stream(inputStream, inFile.getSize(), -1).build());
        //给新文件添加文件描述
        if (!desc.isEmpty()) {
            HashMap<String, String> tags = new HashMap<>();
            tags.put("fileDesc", desc);
            minioClient.setObjectTags(SetObjectTagsArgs.builder().bucket(prodCode).object(inFile.getOriginalFilename()).tags(tags).build());
        }
        infoRes = MinioConfigUtil.minioUrl + "/" + prodCode + "/" + inFile.getOriginalFilename();
        logger.info("=================文件上传：上传地址为 " + infoRes);
        //如果hash一样，minio是不会做替代处理，所以在这将hash一样的图片删除掉
        if (!fileExist.isEmpty() && fileHash.equals(fileExist.get(0).getFileHash())) {
            minioClient.removeObject(RemoveObjectArgs.builder().bucket(prodCode).object(fileExist.get(0).getFileName()).build());
        }
        res.put("STATUS", "S");
        res.put("MSG", infoRes);
        //上传成功记录数据库
        LogMinioOperate uploadParam = new LogMinioOperate();
        uploadParam.setFileName(inFile.getOriginalFilename());
        uploadParam.setIsDelete(CommonEnum.FALSE.getStatusVal());
        uploadParam.setAddrMinio(infoRes);
        uploadParam.setSourceService(prodCode);
        uploadParam.setFileHash(fileHash);
        uploadParam.setFileDesc(desc.isEmpty() ? null : desc);
        uploadParam.setOperate(FileOperateEnum.UPLOAD.getVal());
        if (isReplace && !fileExist.isEmpty()) {
            //覆盖操作，更新数据库
            uploadParam.setReplaceTime(new Date());
            uploadParam.setModifyUser("456");
            uploadParam.setModifyTime(new Date());
            uploadParam.setId(fileExist.get(0).getId());
            //modify
            logMinioOperateService.update(uploadParam);
        } else {
            uploadParam.setCreateUser("123");
            uploadParam.setCreateTime(new Date());
            //insert
            logMinioOperateService.insert(uploadParam);
        }
        return res;
    }
}
