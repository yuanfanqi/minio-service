package com.minioservice.controller;

import com.minioservice.commonModel.ResultRes;
import com.minioservice.service.MinioBaseService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @Author: song
 * @Date: 2023/2/3 10:56
 * @Description:
 */
@RestController
@CrossOrigin
@RequestMapping(value = "/minio", produces = "application/json;charset=UTF-8")
public class MinioCommonController {

    private final static Logger logger = LoggerFactory.getLogger(MinioCommonController.class);

    @Autowired
    MinioBaseService minioBaseService;

    /**
     * @param: [inFile, prodCode, token]
     * @return: java.lang.String
     * @Author: song
     * @Date: 2023/2/3 11:49
     * @Description: 文件上传接口
     */
    @PostMapping("/minioUpload")
    @ResponseBody
    public ResultRes upload (@RequestParam("inFile") MultipartFile inFile,
                             @RequestParam("prodCode") String prodCode,
                             @RequestParam("token") String token,
                             @RequestParam(value = "tags",required = false) String tags,
                             @RequestParam(value = "isReplace", defaultValue = "0") boolean isReplace
    ) {
        logger.info("===================进入文件上传接口===================");
        //todo 参数验证
        //todo tags是否为中文
        try {
            return minioBaseService.doUpload(inFile, prodCode, token, tags, isReplace);
        } catch (Exception e) {
            logger.error("文件上传失败：" + e.getMessage());
            return ResultRes.fail("文件上传失败：" + e.getMessage());
        }
    }

    /**
     * @Description: 文件删除接口
     * @param: [fileName(文件名需带文件类型后缀), prodCode, token, isLogicDel]
     * @return: ResultRes
     * @Author: song
     * @Date: 2023/2/7 16:56
     */
    @PostMapping("/removeFile")
    @ResponseBody
    public ResultRes removeFile (
            @RequestParam("fileName") String fileName,
            @RequestParam("prodCode") String prodCode,
            @RequestParam("token") String token,
            @RequestParam(value = "isLogicDel", defaultValue = "1") boolean isLogicDel
    ) {
        logger.info("===================进入文件删除接口===================");
        //todo token验证
        try {
            return minioBaseService.removeFile(fileName, prodCode, token, isLogicDel);
        } catch (Exception e) {
            logger.error("文件删除失败：" + e.getMessage());
            return ResultRes.fail("文件删除失败：" + e.getMessage());
        }
    }

    /**
     * @Description: 文件列表查询
     * @param: [prodCode, token]
     * @return: java.util.Map<java.lang.String,java.lang.Object>
     * @Author: song
     * @Date: 2023/2/7 17:20
     */
    @PostMapping("/bucketFileList")
    @ResponseBody
    public Map<String, Object> bucketFileList(
            @RequestParam("prodCode") String prodCode,
            @RequestParam("token") String token
    ) {
        logger.info("===================进入文件列表查询接口===================");
        Map<String, Object> res = new HashMap<String, Object>();
        try {
            List<String> fileList = minioBaseService.getBucketFileList(prodCode, token);
            res.put("STATUS", "S");
            res.put("MSG", fileList);
        } catch (Exception e) {
            res.put("STATUS", "E");
            res.put("MSG", "查询失败：" + e.getMessage());
        }
        logger.info("===================文件列表查询接口结束===================");
        return res;
    }

    /**
     * @Description: 文件下载
     * @param: [fileName, prodCode, downloadPath, token]
     * @return: ResultRes
     * @Author: song
     * @Date: 2023/2/10 10:29
     */
    @PostMapping("/doDownload")
    @ResponseBody
    public ResultRes doDownload(
            @RequestParam("fileName") String fileName,
            @RequestParam("prodCode") String prodCode,
            @RequestParam("downloadPath") String downloadPath,
            @RequestParam("token") String token
    ) {
        logger.info("===================进入文件下载接口===================");
        try {
            return minioBaseService.doDownload(fileName, prodCode, token, downloadPath);
        } catch (Exception e) {
            logger.error("下载失败：" + e.getMessage());
            return ResultRes.fail("下载失败：" + e.getMessage());
        }
    }

    /**
     * @Description: 多文件上传
     * @param: [inFileList, prodCode, token, tags, isReplace]
     * @return: java.util.Map<java.lang.String,java.lang.String>
     * @Author: song
     * @Date: 2023/2/9 13:30
     */
    @PostMapping("/minioUploadFiles")
    @ResponseBody
    public ResultRes uploadFiles (@RequestParam("inFileList") MultipartFile[] inFileList,
                                       @RequestParam("prodCode") String prodCode,
                                       @RequestParam("token") String token,
                                       @RequestParam(value = "tags", required = false) String tags,
                                       @RequestParam(value = "isReplace", defaultValue = "0") boolean isReplace
    ) {
        logger.info("===================进入多文件上传接口===================");
        //todo 参数验证
        //todo tags是否为中文
        try {
            return minioBaseService.doUploadFiles(inFileList, prodCode, token, tags, isReplace);
        } catch (Exception e) {
            logger.error("多文件上传失败：" + e.getMessage());
            return ResultRes.fail("多文件上传失败：" + e.getMessage());
        }
    }

    /**
     * @Description: 多文件删除
     * @param: [fileArr, prodCode, token, isLogicDel]
     * @return: com.minioservice.commonModel.ResultRes
     * @Author: song
     * @Date: 2023/2/9 16:18
     */
    @PostMapping("/removeFileList")
    @ResponseBody
    public ResultRes removeFileList (
            @RequestParam("fileList") List<String> fileList,
            @RequestParam("prodCode") String prodCode,
            @RequestParam("token") String token,
            @RequestParam(value = "isLogicDel", defaultValue = "1") boolean isLogicDel
    ) {
        logger.info("===================进入文件删除接口===================");
        //todo token验证
        try {
            return minioBaseService.removeFileList(fileList, prodCode, token, isLogicDel);
        } catch (Exception e) {
            logger.info("删除失败：" + e.getMessage());
            return ResultRes.fail("删除失败：" + e.getMessage());
        }
    }

    /**
     * @Description: 多文件下载
     * @param: [fileList, prodCode, downloadPath, token]
     * @return: com.minioservice.commonModel.ResultRes
     * @Author: song
     * @Date: 2023/2/10 13:27
     */
    @PostMapping("/doDownloadFiles")
    @ResponseBody
    public ResultRes doDownloadFiles(
            @RequestParam("fileList") List<String> fileList,
            @RequestParam("prodCode") String prodCode,
            @RequestParam("downloadPath") String downloadPath,
            @RequestParam("token") String token
    ) {
        logger.info("===================进入多文件下载接口===================");
        try {
            return minioBaseService.doDownloadFiles(fileList, prodCode, token, downloadPath);
        } catch (Exception e) {
            logger.info("下载失败：" + e.getMessage());
            return ResultRes.fail("下载失败：" + e.getMessage());
        }
    }

    /**
     * @Description: 文件tags的修改（暂时支持minio和数据库的同步修改，不含中文）
     * @param: [fileName, prodCode, token]
     * @return: com.minioservice.commonModel.ResultRes
     * @Author: song
     * @Date: 2023/2/10 13:55
     */
    @PostMapping("/modifyFileTags")
    @ResponseBody
    public ResultRes modifyFileTags (
            @RequestParam("fileName") String fileName,
            @RequestParam("prodCode") String prodCode,
            @RequestParam("token") String token,
            @RequestParam("tags") String tags
            ) {
        logger.info("===================进入文件tags修改接口===================");
        return minioBaseService.modifyTags(fileName, prodCode, token, tags);
    }
}
